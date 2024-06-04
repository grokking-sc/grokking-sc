{- |
Module      : Core.Eval
Description : Evaluate statements in the core language

This module implements an evaluator for the core language.
This evaluator presupposes that terms have already been focused.
-}
module Core.Eval (
    eval,
    evalMain,
) where

import Core.Substitution
import Core.Syntax
import Data.List (find)
import Data.Text qualified as T
import Fun.Syntax (BinOp (..))

-- | Evaluate a statement and return the trace containing all individual steps.
eval :: Statement -> Program a -> [Statement]
eval start prg = eval' start [start]
  where
    eval' st acc = case evalOnce st prg of
        Nothing -> acc
        Just st' -> eval' st' (acc ++ [st'])

-- | Evaluate a statement in a program by one step.
evalOnce :: Statement -> Program a -> Maybe Statement
--
-- ⟨μ x.st | c⟩ ▹ st[c/x]
--
{-
>>> evalOnce (Cut (Mu (Covar "a") (Cut (Lit 1) (Covar "a"))) (Covar "b")) (MkProgram [])
(Cut (Lit 1) (Covar "b"))
-}
evalOnce (Cut (Mu cv st) c) _ = Just (substCovar c cv st)
--
-- ⟨p | μ~ x.st⟩ ▹ st[p/x]
--
{-
>>> evalOnce (Cut (Lit 1) (MuTilde (Var "x") (BinOp (Var "x") Sum (Lit 1) (Covar "a")))) (MkProgram [])
BinOp (Lit 1) Sum (Lit 1) (Covar "a")
-}
evalOnce (Cut p (MuTilde v st)) _ = Just (substVar p v st)
--
-- ⟨ctor(pargs;cargs) | case { ... ctor(vars;covars) ⇒ st }⟩ ▹ st[pargs/vars,cargs/covars]
--
{-
>>> evalOnce (Cut (Constructor Nil [] []) (Case [(MkPattern Nil [] Done),(MkPattern Cons ["x","xs"] (Cut (Var "x") (Covar "b")))])) (MkProgram [])
Done
-}
evalOnce (Cut (Constructor ct pargs cargs) (Case patterns)) _ = do
    MkPattern _ vars cvars st <- find (\pat -> xtor pat == ct) patterns
    Just (substSim (zip pargs vars) (zip cargs cvars) st)
--
--  ⟨cocase { ... dtor(vars;covars) ⇒ st } | dtor(pargs;cargs)⟩ ▹ st[pargs/vars,cargs/covars]
--
{-
>>> evalOnce (Cut (Cocase [(MkPattern {Fst ["a"] (Cut (Lit 1) (Covar "a"))}) (MkPattern {Snd ["b"] (Cut (Lit 2) (Covar "b"))})]) (Destructor Fst [] [(Covar "c")])) (MkProgram [)]
 Cut (Lit 1) (Covar "c")
-}
evalOnce (Cut (Cocase patterns) (Destructor dt pargs cargs)) _ = do
    MkPattern _ vars cvars st <- find (\pat -> xtor pat == dt) patterns
    Just (substSim (zip pargs vars) (zip cargs cvars) st)
--
-- ⊙(n,m,c) ▹  ⟨n⊙m | c⟩
--
{-
>>> evalOnce (Op (Lit 2) Prod (Lit 2) (Covar "a")) (MkProgram [])
Cut (Lit 4) (Covar "a")
-}
evalOnce (Op (Lit n) Prod (Lit m) c) _ = Just (Cut (Lit (n * m)) c)
{-
>>> evalOnce (Op (Lit 2) Sum (Lit 3) (Covar "a")) (MkProgram [])
Cut (Lit 5) (Covar "a")
-}
evalOnce (Op (Lit n) Sum (Lit m) c) _ = Just (Cut (Lit (n + m)) c)
{-
>>> evalOnce (Op (Lit 4) Sub (Lit 2) (Covar "a")) (MkProgram [])
Cut (Lit 2) (Covar "a")
-}
evalOnce (Op (Lit n) Sub (Lit m) c) _ = Just (Cut (Lit (n - m)) c)
--
-- ifz(0,s1,s2) ▹ s1
-- ifz(n,s1,s2) ▹ s2
--
{-
>>> evalOnce (IfZ (Lit 0) Done (Cut (Lit 1) (Covar "a"))) (MkProgram [])
Done
>>> evalOnce (IfZ (Lit 1) Done (Cut (Lit 1) (Covar "a"))) (MkProgram [])
(Cut (Lit 1) (Covar "a"))
-}
evalOnce (IfZ (Lit n) s1 s2) _
    | n == 0 = Just s1
    | otherwise = Just s2
--
-- f(ts;as) ▹ t[ts/xs;as/ys] if f(xs;ys) in P
--
{-
>>> evalOnce (Fun "Exit" [] []) (MkProgram [Def { "Exit" [] [] Done }])
Done
-}
evalOnce (Fun nm pargs cargs) (MkProg dfs) = do
    (Def _ vars cvars body) <- find (\def -> name def == nm) dfs
    Just (substSim (zip pargs (fst <$> vars)) (zip cargs (fst <$> cvars)) body)
evalOnce _ _ = Nothing

-- | Evaluate the main statement of a program.
evalMain :: Program a -> Maybe [Statement]
evalMain prg@(MkProg defs) = do
    main <- find (\def -> name def == T.pack "main") defs
    -- ★ is the toplevel continuation
    let bd = substCovar (Covar (T.pack "★")) (fst . head . cargs $ main) (body main)
    pure (eval bd prg)
