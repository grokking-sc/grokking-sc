{-|
Module      : Fun.Parser
Description : Parser for the surface language Fun

This module implements the parser for the surface language Fun.
The parser is implemented using the parser combinator library "Megaparsec".
-}
{-# LANGUAGE OverloadedStrings #-}
module Fun.Parser (parseTerm, parseProg) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Fun.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- a parser with no custom error component (Void) for parsing text
type Parser = Parsec Void Text

-- Lexing

-- space parser
-- parses spaces and comments
-- either line comments starting with // or block comments enclosed by /* */
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

keywords :: [Text]
keywords =
    -- constructor for empty list
    [ "Nil"
    -- constructor for list with head and tail
    , "Cons"
    -- constructor for pairs
    , "Tup"
    -- destructor for stream, returning the head of a stream
    , "hd"
    -- destructor for stream, returning the tail of a stream
    , "tl"
    -- destructor for lazy pair, returning the first element
    , "fst"
    -- destructor for lazy pair, returning the second element
    , "snd"
    -- if zero term
    , "ifz"
    -- let-in
    , "let"
    , "in"
    -- case expressions
    , "case"
    , "of"
    -- cocase expression (also uses "of")
    , "cocase"
    -- defining goto and labels
    , "goto"
    , "label"
    -- defining top level definitions
    , "def"
    ]

-- checks if a name can be used
-- fails the parser if the given text is a keyword, otherwise returns
checkReserved :: Text -> Parser ()
checkReserved str
    | str `elem` keywords = fail ("Keyword " <> T.unpack str <> " cannot be used as an identifier.")
    | otherwise = return ()

-- parses a given symbol (of type Text)
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- parses (, then runs a given parser, then parses )
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- parses {, then runs a given parser, then parses }
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- constructor name parser
-- parses wither Nil, Cons or Tup
ctorP :: Parser Ctor
ctorP = nilP <|> consP <|> tupP
  where
    nilP = symbol "Nil" >> pure Nil
    consP = symbol "Cons" >> pure Cons
    tupP = symbol "Tup" >> pure Tup

-- destructor name parser
-- parses hd,tl, fst or snd
dtorP :: Parser Dtor
dtorP = hdP <|> tlP <|> fstP <|> sndP
  where
    hdP = symbol "hd" >> pure Hd
    tlP = symbol "tl" >> pure Tl
    fstP = symbol "fst" >> pure Fst
    sndP = symbol "snd" >> pure Snd

-- binary operation parser
-- parses eithr *,+ or -
binOpP :: Parser BinOp
binOpP = prodP <|> sumP <|> subP
  where
    prodP = symbol "*" >> pure Prod
    sumP = symbol "+" >> pure Sum
    subP = symbol "-" >> pure Sub

-- argument parser
-- parses a list of terms separated by , and enclosed by ( )
argsP :: Parser [Term]
argsP = parens (sepEndBy termP' (symbol ","))

-- variable parser
-- parses a number of variables (identifier) separated by , and enclosed by ( )
varsP :: Parser [Var]
varsP = parens (sepEndBy (identifierP <* sc) (symbol ","))

-- identifier parser
-- used for variables  and new names
-- parses any number of alphanumeric characters, then checks if the parsed name is reserved
-- the <?> is used for error propagation
identifierP :: Parser Text
identifierP = do
    str <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
    checkReserved str
    pure str

-- Parsing Terms
-- variable parser
-- parses only an identifier
variableP :: Parser Term
variableP = do
    var <- identifierP
    sc
    pure (VarT var)

-- integer literal parser
-- parses any decimal
litP :: Parser Term
litP = do
    n <- L.decimal
    sc
    pure (Lit n)

-- if zero parser
-- ifz(t1,t2,t3)
ifzP :: Parser Term
ifzP = do
    -- start by parsing the ifz keyword
    _ <- symbol "ifz"
    -- parse the arguments
    (tm1, tm2, tm3) <- parens $ do
        -- arguments are three terms separated by commas
        tm1 <- termP'
        _ <- symbol ","
        tm2 <- termP'
        _ <- symbol ","
        tm3 <- termP'
        pure (tm1, tm2, tm3)
    pure (IfZ tm1 tm2 tm3)

-- let-in parser
-- let v = t1 in t2
letP :: Parser Term
letP = do
    -- first parse "let"
    _ <- symbol "let"
    -- then parse the bound variable
    var <- identifierP
    sc
    -- parse "=" and the term bound to the variable
    _ <- symbol "="
    tm <- termP'
    -- parse "in" and the term the variable is bound in
    _ <- symbol "in"
    tm' <- termP'
    pure (Let var tm tm')

-- parse toplevel definition call
-- fun(args)
funP :: Parser Term
funP = do
    -- parse the name of the definition
    name <- identifierP
    -- parse the arguments
    args <- funArgsP
    pure $ uncurry (Fun name) args

-- argument parser for toplevel calls
-- (args;cv) or (args)
funArgsP :: Parser ([Term],Maybe Covar)
funArgsP = do
  -- arguments are enclsed by ()
  _ <- symbol "("
  sc
  -- arguments are terms separated by comas
  args <- sepEndBy termP' (symbol ",")
  sc
  -- optionally parse the continuation covariable
  -- this is needed to propagate labels for gotos
  -- the covariable is separated by the other arguments by ;
  cv <- optional (symbol ";" >> identifierP)
  _ <- symbol ")"
  pure (args,cv)

-- constructor term parser
-- ctor(args)
conP :: Parser Term
conP = do
    -- parse constructor name using ctorP defined above
    ctor <- ctorP
    -- optionally parse arguments
    -- Nil has no  arguments, so option is needed
    -- this uses the argsP parser defined above for arguments
    args <- option [] argsP
    pure (ConT ctor args)

-- parser for patterns in cases and cocases
-- the type variable a parameterizes constructors and destructors
-- the parser for a needs to be given as an argument
-- i.e. case patterns are  parsed using (clauseP ctorP) and cocase patterns using (clauseP dtorP) (see below)
-- ctor(vars) => t
clauseP :: Parser a -> Parser (Clause a)
clauseP p = do
    -- parse the xtor (either constructor or destructor, depending on the argument)
    xtor <- p
    -- parse variables bound by pattern
    -- option is again needed because Nil has no arguments
    vars <- option [] varsP
    -- parse => and the term bound in the pattern
    _ <- symbol "=>"
    tm <- termP'
    pure (MkClause xtor vars tm)

-- parser for cases
-- case t of { pts }
caseP :: Parser Term
caseP = do
    -- first parse case t of
    -- with t being the scrutinee term
    _ <- symbol "case"
    e <- termP'
    _ <- symbol "of"
    -- then parse any number of patterns enclosed by { } and separated by ,
    -- this used the above clauseP parser with the above constructor name parser
    cases <- braces (sepEndBy (clauseP ctorP) (symbol ","))
    pure (Case e cases)

-- parser for cocases
-- cocase { pts }
cocaseP :: Parser Term
cocaseP = do
    -- parse "cocase" followed by any number of patterns enclosed by {} and separated by ,
    -- cocases have no scrutinee term, so we immediately parse patterns after the "cocase"-keyword
    -- this again uses the clauseP parser above with the destructor name parser dtorP
    _ <- symbol "cocase"
    cocases <- braces (sepEndBy (clauseP dtorP) (symbol ","))
    pure (Cocase cocases)

-- parser for lambda abstractions
-- \\var => t
lamP :: Parser Term
lamP = do
    -- parses a backslash, followed by a variable, then followed by => and finished by the function body tm
    _ <- symbol "\\"
    var <- identifierP
    sc
    _ <- symbol "=>"
    tm <- termP'
    pure (Lam var tm)

-- parser for Gotos
-- goto(tm,var)
gotoP :: Parser Term
gotoP = do
    -- parses the goto keyword followed by the label and contained term
    _ <- symbol "goto"
    -- the varable and term are enclosed by ( ) and separated by ,
    (var, tm) <- parens $ do
        tm <- termP'
        _ <- symbol ","
        var <- identifierP
        sc
        pure (var, tm)
    pure (Goto tm var)

-- parser for labels
-- label v {t}
labelP :: Parser Term
labelP = do
    -- parses the "label"-keyword
    -- then the label name and the term in which the label is in scope enclosed by { }
    _ <- symbol "label"
    var <- identifierP
    sc
    tm <- braces termP'
    pure (Fun.Syntax.Label var tm)

-- parses a term enclosed by  ()
termParensP :: Parser Term
termParensP = parens termP'

-- Left recursive parsers
-- all these parsers start by parsing another term
-- these have to be handled separately to avoid infinite recursion
-- see termP and termP' below

-- parser for function application
-- parses two terms after one another
-- t1 t2
appP :: Parser Term
appP = do
    tm1 <- termP
    tm2 <- termP'
    pure (App tm1 tm2)

-- parser for destructors
-- dtor(args)
desP :: Parser Term
desP = do
    -- parses the term to be destructed
    -- followed by "." and optionally the destructor arguments
    tm <- termP
    _ <- string "."
    dtor <- dtorP
    -- since our only destructors are fst, snd, hd and tl, these should always be empty
    -- in the language there is also the ap destructor for functions (replacing lambda abstractions)
    -- it is not included in the parser, but could be added by adding "ap" to keywords and the dtorP parser
    args <- option [] argsP
    pure (DesT tm dtor args)

-- parses a binary operation
-- in duality, these are inline, e.g. 3*4
-- t2 (x) t2
opP :: Parser Term
opP = do
    -- parse the first argument term
    -- then parse the binary operation (binOpP defined above)
    -- parse the second argument term
    tm1 <- termP
    bop <- binOpP
    tm2 <- termP'
    pure (Op tm1 bop tm2)

-- Combined parser for terms
-- the termP parser only parses non-left-recursive terms
-- that is anything that is not a destructor, function application or binary operation
-- the termP' parser then uses the termP parser to parse any possible term

-- parse any non-left-recursive term
termP :: Parser Term
termP =
    -- try parsing toplevel call
    -- this needs to be try, as toplevel calls start with the name
    -- the parser could fail but already have consumed a name, so it needs to backtrack on fail
    try funP
    --try parsing a variable
    --this also needs to try as the parser could fail after consuming a name
        <|> try variableP
        -- parse literal
        <|> litP
        -- parse constructor
        <|> conP
        -- parse case
        <|> caseP
        -- parse cocase
        <|> cocaseP
        -- parse goto
        <|> gotoP
        -- parse label
        <|> labelP
        -- parse ifzero
        <|> ifzP
        -- parse let-in
        <|> letP
        -- parse lambda abstraction
        <|> lamP
        -- parse any other term enclosed by ()
        -- this parser uses termP', so any term can be enclosed in parentheses
        -- this also means termP and termP' are mutually recursive
        <|> termParensP

-- parse any term
-- first try any of the left-recursive terms, if they all fail, parse one of the others
-- all the left-recursive parsers need backtracking, as they all start by consuming a term
-- if they fail that term would be skipped without "try"
termP' :: Parser Term
termP' = try desP <|> try appP <|> try opP <|> termP

-- Program parser
-- parse a toplevel definition
-- the definition has type argument ()
-- the type argument is used for the argument types
-- since these are only added by the type checker, these are blank for now
-- def name(args) := t;
defP :: Parser (Def ())
defP = do
    -- parse the "def" keyword, the name of the definition
    _ <- symbol "def"
    name <- identifierP
    -- parse the variable arguments along with the optional covariable argument
    -- the covariable argument is only needed for labels/gotos to propagate a label
    (args,cv) <- option ([],Nothing) defArgsP
    sc
    -- parse ":=" followed by the body of the definition
    _ <- symbol ":="
    body <- termP'
    -- definitions need to be ended by a semicolon
    _ <- symbol ";"
    pure (Def name ((\x -> (x, ())) <$> args) cv body ())

-- parse arguments for a toplevel definition
-- (args) or (args;cv)
defArgsP :: Parser ([Var],Maybe Covar)
defArgsP = do
  -- arguments are enclosed by ( )
  _ <- symbol "("
  sc
  -- argument variables are any identifier and separated by ,
  args <- sepEndBy identifierP (symbol ",")
  sc
  -- if the definition uses a covariable argument, it needs to come after the variable arguments and be separated from them by ;
  cv <- optional (symbol ";" >> identifierP)
  _ <- symbol ")"
  pure (args,cv)


-- parse a program
-- parses any number of toplevel definitions
progP :: Parser (Prog ())
progP = MkProg <$> many defP

-- Exported
-- parseTerm and parseProg both run a parser
-- either for parsing a single term or for parsing a program
-- either returns the pretty printed error if the parser fails or returns the parsed term/program
parseTerm :: Text -> Either String Term
parseTerm input = case runParser (sc >> termP' <* eof) "<interactive>" input of
    Left err -> Left (errorBundlePretty err)
    Right tm -> Right tm

parseProg :: Text -> Either String (Prog ())
parseProg input = case runParser (sc >> progP <* eof) "<interactive>" input of
    Left err -> Left (errorBundlePretty err)
    Right prog -> Right prog
