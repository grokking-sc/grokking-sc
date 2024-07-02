{- |
Module      : Core.Simplify
Description : Simplify expressions by reducing administrative redexes.

This module contains a very primitive simplification algorithm
which reduces administrative redexes.
-}
module Core.Simplify (
    Simplify,
    simplify,
) where

import Core.Substitution
import Core.Syntax

-- | Type class for simplifying expressions in the core language
class Simplify a where
    -- | Simplify an expression
    simplify :: a -> a

instance Simplify (Program a) where
    simplify (MkProg dfs) = MkProg (simplify <$> dfs)

instance Simplify (Def a) where
    simplify Def{name = nm, pargs = args, cargs = coargs, body = st} = Def{name = nm, pargs = args, cargs = coargs, body = simplify st}

instance Simplify Statement where
    -- an administrative mu abstraction in a cut can immediately be removed by substituting the consumer
    -- <mu cv.st | c> -> st [c/cv]
    {-
     >>> simplify (Cut (Mu "a" (Cut (Lit 1) (Covar "a")) (Covar "b"))
     Cut (Lit 1) (Covar "b")
     -}
    simplify (Cut (Mu cv1 s) c) = simplify (substCovar c cv1 s)
    -- as with a mu abstration, an administrative mu-tilde abstraction can be simplified by substituting the producer, unless it is a non-administrative mu
    -- <p | ~mu v.st> -> st[p/v]
    {-
     >>> simplify (Cut (Var "x") (MuTilde "y" (Cut (Var "y") (Covar "a")))
     Cut (Var "x") (Covar "a")
     -}
    simplify (Cut (MuDyn cv1 s1) (MuTilde v2 s2)) = Cut (MuDyn cv1 (simplify s1)) (MuTilde v2 (simplify s2))
    simplify (Cut p (MuTilde v2 s)) = simplify (substVar p v2 s)
    -- in all other cases, just recursively simplify all subterms
    simplify (Cut p c) = Cut (simplify p) (simplify c)
    simplify (Op p1 op p2 c) = Op (simplify p1) op (simplify p2) (simplify c)
    simplify (IfZ p s1 s2) = IfZ (simplify p) (simplify s1) (simplify s2)
    simplify (Fun nm args coargs) = Fun nm (simplify <$> args) (simplify <$> coargs)
    simplify Done = Done

instance Simplify (Pattern a) where
    simplify MkPattern{xtor = xt, patv = vars, patcv = covars, patst = st} = MkPattern{xtor = xt, patv = vars, patcv = covars, patst = simplify st}

instance Simplify Producer where
    simplify (Var v) = Var v
    simplify (Lit n) = Lit n
    simplify (Mu cv st) = Mu cv (simplify st)
    simplify (MuDyn cv st) = MuDyn cv (simplify st)
    simplify (Constructor ct args coargs) = Constructor ct (simplify <$> args) (simplify <$> coargs)
    simplify (Cocase pts) = Cocase (simplify <$> pts)

instance Simplify Consumer where
    simplify (Covar cv) = Covar cv
    simplify (MuTilde v st) = MuTilde v (simplify st)
    simplify (MuTildeDyn v st) = MuTildeDyn v (simplify st)
    simplify (Case pts) = Case (simplify <$> pts)
    simplify (Destructor dt args coargs) = Destructor dt (simplify <$> args) (simplify <$> coargs)
