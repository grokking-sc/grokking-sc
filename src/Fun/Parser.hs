{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Fun.Parser
Description : Parser for the surface language Fun

This module implements the parser for the surface language Fun.
The parser is implemented using the parser combinator library @Megaparsec@.
-}
module Fun.Parser (parseTerm, parseProgram) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Fun.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | A parser with no custom error component (Void) for parsing text
type Parser = Parsec Void Text

-------------------------------------------------------------------------------
-- Lexing
-------------------------------------------------------------------------------

-- | Parse whitespace and comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- | Reserved keywords that cannot be used as names for variables or toplevel functions.
keywords :: [Text]
keywords =
    [ "Nil"
    , "Cons"
    , "Tup"
    , "hd"
    , "tl"
    , "fst"
    , "snd"
    , "ifz"
    , "let"
    , "in"
    , "case"
    , "of"
    , "cocase"
    , "goto"
    , "label"
    , "def"
    ]

-- | Check if the string is a reserved keyword.
checkReserved :: Text -> Parser ()
checkReserved str
    | str `elem` keywords = fail ("Keyword " <> T.unpack str <> " cannot be used as an identifier.")
    | otherwise = return ()

-- | Parse the given string.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a parenthesized expression
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse an expression between curly braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parse a constructor
ctorP :: Parser Ctor
ctorP = nilP <|> consP <|> tupP
  where
    nilP = symbol "Nil" >> pure Nil
    consP = symbol "Cons" >> pure Cons
    tupP = symbol "Tup" >> pure Tup

-- | Parse a destructor
dtorP :: Parser Dtor
dtorP = hdP <|> tlP <|> fstP <|> sndP
  where
    hdP = symbol "hd" >> pure Hd
    tlP = symbol "tl" >> pure Tl
    fstP = symbol "fst" >> pure Fst
    sndP = symbol "snd" >> pure Snd

-- | Parse binary operators
binOpP :: Parser BinOp
binOpP = prodP <|> sumP <|> subP
  where
    prodP = symbol "*" >> pure Prod
    sumP = symbol "+" >> pure Sum
    subP = symbol "-" >> pure Sub

-- | Parse a parenthesized, comma-separated list of variables.
varsP :: Parser [Var]
varsP = parens (sepEndBy (identifierP <* sc) (symbol ","))

-- | Parse a single identifier.
identifierP :: Parser Text
identifierP = do
    str <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
    checkReserved str
    pure str

-------------------------------------------------------------------------------
-- Parsing Terms
-------------------------------------------------------------------------------

-- | Parse a parenthesized, comma-separated list of terms
argsP :: Parser [Term]
argsP = parens (sepEndBy termP' (symbol ","))

-- | Parse a variable.
variableP :: Parser Term
variableP = do
    var <- identifierP
    sc
    pure (VarT var)

-- | Parse an integer literal.
litP :: Parser Term
litP = do
    n <- L.decimal
    sc
    pure (Lit n)

-- | Parse an if-zero term @ifz(t1,t2,t3)@
ifzP :: Parser Term
ifzP = do
    _ <- symbol "ifz"
    (tm1, tm2, tm3) <- parens $ do
        tm1 <- termP'
        _ <- symbol ","
        tm2 <- termP'
        _ <- symbol ","
        tm3 <- termP'
        pure (tm1, tm2, tm3)
    pure (IfZ tm1 tm2 tm3)

-- | Parse a let-expression @let v = t1 in t2@
letP :: Parser Term
letP = do
    _ <- symbol "let"
    var <- identifierP
    sc
    _ <- symbol "="
    tm <- termP'
    _ <- symbol "in"
    tm' <- termP'
    pure (Let var tm tm')

-- | Parse a toplevel definition call @fun(t1,...,tn)@ or @fun(t1,...,tn;a)@
funP :: Parser Term
funP = do
    name <- identifierP
    args <- funArgsP
    pure $ uncurry (Fun name) args

-- | Parse the argument list @(t1,...,tn;cv)@ or @(t1,...,tn)@ of a toplevel call.
funArgsP :: Parser ([Term], Maybe Covar)
funArgsP = do
    _ <- symbol "("
    sc
    args <- sepEndBy termP' (symbol ",")
    sc
    -- Optionally parse the continuation covariable
    cv <- optional (symbol ";" >> identifierP)
    _ <- symbol ")"
    pure (args, cv)

-- | Parse a constructor applied to a list of arguments.
conP :: Parser Term
conP = do
    ctor <- ctorP
    args <- option [] argsP
    pure (Constructor ctor args)

-- | Parse a single clause for a case or cocase expression.
clauseP :: Parser a -> Parser (Clause a)
clauseP p = do
    xtor <- p
    vars <- option [] varsP
    _ <- symbol "=>"
    tm <- termP'
    pure (MkClause xtor vars tm)

-- | Parse a single case expression @case t of { clauses }@.
caseP :: Parser Term
caseP = do
    _ <- symbol "case"
    e <- termP'
    _ <- symbol "of"
    cases <- braces (sepEndBy (clauseP ctorP) (symbol ","))
    pure (Case e cases)

-- | Parse a single cocase expression @cocase { clauses }@.
cocaseP :: Parser Term
cocaseP = do
    _ <- symbol "cocase"
    cocases <- braces (sepEndBy (clauseP dtorP) (symbol ","))
    pure (Cocase cocases)

-- | Parse a lambda abstraction @\var => t@.
lamP :: Parser Term
lamP = do
    _ <- symbol "\\"
    var <- identifierP
    sc
    _ <- symbol "=>"
    tm <- termP'
    pure (Lam var tm)

-- | Parse a goto expression @goto(tm, var)@.
gotoP :: Parser Term
gotoP = do
    _ <- symbol "goto"
    (var, tm) <- parens $ do
        tm <- termP'
        _ <- symbol ","
        var <- identifierP
        sc
        pure (var, tm)
    pure (Goto tm var)

-- | Parse a labelled expression @label v {t}@.
labelP :: Parser Term
labelP = do
    _ <- symbol "label"
    var <- identifierP
    sc
    tm <- braces termP'
    pure (Fun.Syntax.Label var tm)

-- | Parse a parenthesized term @(t)@.
termParensP :: Parser Term
termParensP = parens termP'

-- The following terms are all left-recursive, i.e. they start with another term.

-- | Parse a function application @t1 t2@.
appP :: Parser Term
appP = do
    tm1 <- termP
    tm2 <- termP'
    pure (App tm1 tm2)

-- | Parse a "destructor chain": @t.dtor(t1,...tn)...dtor(s1,...,sm)@.
destructorChainP :: Parser Term
destructorChainP = do
    tm <- termP
    dtors <- some destructorApp
    pure (mergeDestructorChain tm dtors)
  where
    destructorApp :: Parser (Dtor, [Term])
    destructorApp = do
        _ <- string "."
        dtor <- dtorP
        args <- option [] argsP
        pure (dtor, args)
    mergeDestructorChain :: Term -> [(Dtor, [Term])] -> Term
    mergeDestructorChain tm [] = tm
    mergeDestructorChain tm ((dtor, args) : xs) = mergeDestructorChain (Destructor tm dtor args) xs

{- | Parse a binary operator @t1 + t2@, @t1 * t2@ and @t1 - t2@.
We don't implement different precedences for different binary operators.
-}
opP :: Parser Term
opP = do
    tm1 <- termP
    bop <- binOpP
    tm2 <- termP'
    pure (Op tm1 bop tm2)

-- When we combine the different parsers for individual terms we have to carefully
-- distinguish left-recursive terms from non-left-recursive terms.
-- This is why there are two different parsers 'termP' and 'termP''.

-- | Parse any term which is not left-recursive.
termP :: Parser Term
termP =
    try funP
        <|> try variableP
        <|> litP
        <|> conP
        <|> caseP
        <|> cocaseP
        <|> gotoP
        <|> labelP
        <|> ifzP
        <|> letP
        <|> lamP
        <|> termParensP

-- | Parse any term, including those which are left-recursive.
termP' :: Parser Term
termP' = try destructorChainP <|> try appP <|> try opP <|> termP

-------------------------------------------------------------------------------
-- Parsing toplevel functions and programs
-------------------------------------------------------------------------------

-- | Parse a single toplevel function definition.
defP :: Parser (Def ())
defP = do
    _ <- symbol "def"
    name <- identifierP
    (args, cv) <- option ([], Nothing) defArgsP
    sc
    _ <- symbol ":="
    body <- termP'
    _ <- symbol ";"
    pure (Def name ((\x -> (x, ())) <$> args) cv body ())

-- | Parse arguments @(v1,...,vn)@ or @(v1,...,vn;cv)@ of a toplevel definition.
defArgsP :: Parser ([Var], Maybe Covar)
defArgsP = do
    _ <- symbol "("
    sc
    args <- sepEndBy identifierP (symbol ",")
    sc
    cv <- optional (symbol ";" >> identifierP)
    _ <- symbol ")"
    pure (args, cv)

-- | Parse a program which consists of many toplevel function definitions.
programP :: Parser (Program ())
programP = MkProg <$> many defP

-------------------------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------------------------

-- | Parse a term of the surface language @Fun@.
parseTerm :: Text -> Either String Term
parseTerm input = case runParser (sc >> termP' <* eof) "<interactive>" input of
    Left err -> Left (errorBundlePretty err)
    Right tm -> Right tm

-- | Parse a program of the surface language @Fun@.
parseProgram :: Text -> Either String (Program ())
parseProgram input = case runParser (sc >> programP <* eof) "<interactive>" input of
    Left err -> Left (errorBundlePretty err)
    Right prog -> Right prog
