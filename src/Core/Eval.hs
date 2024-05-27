module Core.Eval (eval, evalMain) where

import Core.Substitution
import Core.Syntax
import Data.List (find)
import Data.Text qualified as T
import Duality.Syntax (BinOp (..))

eval :: Statement -> Prog a -> [Statement]
eval start prg = eval' start [start]
  where
    -- evaluate once and check if evaluation succeeded
    eval' st acc = case evalOnce st prg of
      -- nothing is returned when there is no evaluation rule matching the statement
      -- in this case we are done and either have finished or are stuck
      Nothing -> acc
      -- when we could evaluate once, we repeat 
      Just st' -> eval' st' (acc ++ [st'])

-- evaluate a statement one time
evalOnce :: Statement -> Prog a -> Maybe Statement

-- definition 2.2
-- if the producer of a cut is a mu-abstraction, replace the covariable by the consumer 
-- <mu x.st | c> -> st[c/x]
evalOnce (Cut (Mu cv st) c) _ = Just (substCovar c cv st)

-- definition 2.3
-- if the consumer of a cut is a mu-tilde-abstraction, replace the variable by the producer
-- <p | mu~ x.st> -> st[p/x]
evalOnce (Cut p (MuTilde v st)) _ = Just (substVar p v st)

-- definition 2.5
-- when a cut contains a constructor and case (of the same type), proceed with the matching command
-- <ctor(args) | case { ... ctor(vars) => st }> -> st[args/vars]
-- dual to the <cocase | dtor> case 
evalOnce (Cut (Constructor ct pargs cargs) (Case patterns)) _ = do
    -- first find the pattern matching the ctor
    MkPattern _ vars cvars st <- find (\pat -> xtor pat == ct) patterns
    -- then substitute the pattern variables by the ctor arguments
    Just (substSim (zip pargs vars) (zip cargs cvars) st)
-- when a cut contains a cocase and destructor (of the same type), proceed with the matching command
-- <cocase { ... dtor(vars) => st } | dtor(args)> -> st[args/vars]
-- dual to the <ctor | case> case
evalOnce (Cut (Cocase patterns) (Destructor dt pargs cargs)) _ = do
    -- first find the pattern corresponding to the dtor
    MkPattern _ vars cvars st <- find (\pat -> xtor pat == dt) patterns
    -- then replace the pattern variables by the dtor args
    Just (substSim (zip pargs vars) (zip cargs cvars) st)

-- definition 2.2
-- A binary operation (x) containing two literals (integers) n,m directly evaluates to n (x) m
-- The consumer argument c is then used as the consumer of the resulting command
-- this consumer is the continuation of the binary operation
evalOnce (Op (Lit n) Prod (Lit m) c) _ = Just (Cut (Lit (n * m)) c)
evalOnce (Op (Lit n) Sum (Lit m) c) _ = Just (Cut (Lit (n + m)) c)
evalOnce (Op (Lit n) Sub (Lit m) c) _ = Just (Cut (Lit (n - m)) c)
-- for if zero, check the first argument and return the corresponding second or third argument
evalOnce (IfZ (Lit n) s1 s2) _
    | n == 0 = Just s1
    | otherwise = Just s2

-- definition 2.4
-- For a top-level declaration, look up the definition in the program and replace the variables by arguments
evalOnce (Fun nm pargs cargs) (MkProg dfs) = do
    -- lookup definition of nm
    (Def _ vars cvars body) <- find (\def -> name def == nm) dfs
    -- substitute variables by arguments
    Just (substSim (zip pargs (fst <$> vars)) (zip cargs (fst <$> cvars)) body)
-- if none of the above matches, evaluation cannot proceed
evalOnce _ _ = Nothing


-- main is the entry point, if it is found the program is evaluated from there
evalMain :: Prog a -> Maybe [Statement]
evalMain prg@(MkProg defs) = do
    main <- find (\def -> name def == T.pack "main") defs
    -- to evaluate main, insert the covariable ★ as last covariable argument of main
    -- this covariable indicates a finished computation when it is the consumer of a cut whose producer is a value
    let bd = substCovar (Covar (T.pack "★")) (fst . head . cargs $ main) (body main)
    -- evaluate the main command with the current program
    pure (eval bd prg)
