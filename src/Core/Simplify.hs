{- |
Module      : Core.Simplify
Description : Simplify expressions by reducing redexes.

This module contains a very primitive simplification algorithm
which reduces statically known redexes.
-}
module Core.Simplify (
    Simplify,
    simplify,
) where

import Core.Substitution
import Core.Syntax
import Data.List
import Fun.Syntax (BinOp (..))

-- | Type class for simplifying expressions in the core language
class Simplify a where
    -- | Simplify an expression
    simplify :: a -> a

instance Simplify (Program a) where
    {-
     >>> simplify (MkProg [])
     MkProg []
     -}
    simplify (MkProg dfs) = MkProg (simplify <$> dfs)

instance Simplify (Def a) where
    {-
     >>> simplify (Def "Exit" [] [] Done)
     Def "Exit" [] [] Done
     -}
    simplify Def{name = nm, pargs = args, cargs = coargs, body = st} = Def{name = nm, pargs = args, cargs = coargs, body = simplify st}

{- | Simplify Instance for statements
This uses evaluation rules from Core.Eval to simplify statements
-}
instance Simplify Statement where
    -- a mu abstraction in a cut can immediately be removed by substituting the consumer
    -- <mu cv1.st | c> -> st [c/cv1]
    {-
     >>> simplify (Cut (Mu "a" (Cut (Lit 1) (Covar "a")) (Covar "b"))
     Cut (Lit 1) (Covar "b")
     -}
    simplify (Cut (Mu cv1 s) c) = simplify (substCovar c cv1 s)
    -- as with a mu abstration, a mu-tilde abstraction can be simplified by substituting the producer
    -- <p | mu v.st> -> st[p/v]
    {-
     >>> simplify (Cut (Var "x") (MuTilde "y" (Cut (Var "y") (Covar "a")))
     Cut (Var "x") (Covar "a")
     -}
    simplify (Cut p (MuTilde v2 s)) = simplify (substVar p v2 s)
    -- a cut between constructor and case can be replaced by the corresponding right-hand side of the pattern
    -- <ctor(args) | case { ... ctor(vars) => st ...} -> st[args/vars]
    {-
     >>> simplify (Cut (Constructor Cons [Lit 1,Nil] []) (Case [MkPattern Nil [] [] Done, MkPattern Cons ["x","xs"] [] (Mu "a" (Var "x"))]))
     Mu "a" (Lit 1)

     >>> simplify (Cut (Constructor Nil [] []) (Case []))
     Cut (Constructor Nil [] []) (Case [])
     -}
    simplify (Cut (Constructor ct args coargs) (Case pts)) = case find (\pat -> xtor pat == ct) pts of
        Nothing -> Cut (Constructor ct (simplify <$> args) (simplify <$> coargs)) (Case (simplify <$> pts))
        Just MkPattern{xtor = _, patv = vars, patcv = covars, patst = st} -> substSim (zip args vars) (zip coargs covars) st
    -- as with a constructor and case, a cocase and destructor can similarly be simplified
    {-
     >>> simplify (Cut (Cocase [MkPattern Fst [] ["a"] (Cut (Lit 1) (Covar "a")), MkPattern Snd [] ["b"] (Cut (Lit 2) (Covar "b"))]) (Destructor Fst [] [Covar "c"]))
     Cut (Lit 1) (Covar "c")

     >>> simplify (Cut (Cocase []) (Destructor Fst [] [Covar "a"])
     Cut (Cocase []) (Destructor Fst [] (Covar "a"))
     -}
    simplify (Cut (Cocase pts) (Destructor dt args coargs)) = case find (\pat -> xtor pat == dt) pts of
        Nothing -> Cut (Cocase (simplify <$> pts)) (Destructor dt (simplify <$> args) (simplify <$> coargs))
        Just MkPattern{xtor = _, patv = vars, patcv = covars, patst = st} -> substSim (zip args vars) (zip coargs covars) st
    -- otherwise simplifiy the producer and consumer in the cut
    {-
     >>> simplify (Cut (Lit 1) (Covar "a"))
     Cut (Lit 1) (Covar "a")
     -}
    simplify (Cut p c) = Cut (simplify p) (simplify c)
    -- binary operations in literals can be immediately computed
    {-
     >>> simplify (Op (Lit 2) Prod (Lit 2) (Covar "a"))
     Cut (Lit 4) (Covar "a")
     -}
    simplify (Op (Lit n) Prod (Lit m) c) = Cut (Lit (n * m)) c
    {-
     >>> simplify (Op (Lit 1) Sum (Lit 1) (Covar "a"))
     Cut (Lit 2) (Covar "a")
    -}
    simplify (Op (Lit n) Sum (Lit m) c) = Cut (Lit (n + m)) c
    {-
     >>> simplify (Op (Lit 3) Sub (Lit 1) (Covar "a"))
     Cut (Lit 2) (Covar "a")
    -}
    simplify (Op (Lit n) Sub (Lit m) c) = Cut (Lit (n - m)) c
    -- otherwise simplify the arguments
    {-
     >>> simplify (Op (Mu "x" Done) Sum (Mu "x" Done) (Covar "a"))
     Op (Mu "x" Done) Sum (Mu "x" Done) (Covar "a")
     -}
    simplify (Op p1 op p2 c) = Op (simplify p1) op (simplify p2) (simplify c)
    -- ifzero 0 can be direclty shortcut to the first statement
    {-
     >>> simplify (IfZ (Lit 0) Done (Cut (Lit 1) (Covar "a") (Covar "b")))
     Done
    -}
    simplify (IfZ (Lit 0) s1 _) = simplify s1
    -- ifzero n /= 0 can be simplified to the second statemetn
    {-
     >>> simplify (IfZ (Lit 1) Done (Cut (Lit 1) (Covar "a") (Covar "b")))
     (Cut (Lit 1) (Covar "a'))
    -}
    simplify (IfZ (Lit _) _ s2) = simplify s2
    -- otherwise simplify all arguments
    {-
     >>> simplify (IfZ (Mu "x" (Cut (Lit 1) (Var "x"))) Done Done)
     IfZ (Mu "x" Cut (Lit 1) (Var "x")) Done Done
     -}
    simplify (IfZ p s1 s2) = IfZ (simplify p) (simplify s1) (simplify s2)
    -- for a toplevel call, simplify all arguments
    -- this does not replace the call with it's body
    {-
     >>> simplify (Fun "Exit" [] [])
     Fun "Exit" [] []
    -}
    simplify (Fun nm args coargs) = Fun nm (simplify <$> args) (simplify <$> coargs)
    {-
     >>> simplify Done
     Done
    -}
    simplify Done = Done

instance Simplify (Pattern a) where
    {-
     >>> simplify (MkPattern Nil [] [] Done)
     MkPattern Nil [] [] Done
    -}
    simplify MkPattern{xtor = xt, patv = vars, patcv = covars, patst = st} = MkPattern{xtor = xt, patv = vars, patcv = covars, patst = simplify st}

instance Simplify Producer where
    {-
     >>>simplify (Var "x")
     Var "x"
    -}
    simplify (Var v) = Var v
    {-
     >>> simplify (Lit 1)
     Lit 1
     -}
    simplify (Lit n) = Lit n
    {-
     >>> simplify (Mu "a" Done)
     Mu "a" Done
     -}
    simplify (Mu cv st) = Mu cv (simplify st)
    {-
     >>> simplify (Constructor Nil [] [])
     Constructor Nil [] []
     -}
    simplify (Constructor ct args coargs) = Constructor ct (simplify <$> args) (simplify <$> coargs)
    {-
     >>> simplify (Cocase [])
     Cocase []
     -}
    simplify (Cocase pts) = Cocase (simplify <$> pts)

instance Simplify Consumer where
    {-
     >>> simplify (Covar "a")
     Covar "a"
     -}
    simplify (Covar cv) = Covar cv
    {-
     >>> simplify (MuTilde "x" Done)
     MuTilde "x" Done
     -}
    simplify (MuTilde v st) = MuTilde v (simplify st)
    {-
     >>> simplify (Case [])
     Case []
     -}
    simplify (Case pts) = Case (simplify <$> pts)
    {-
     >>> simplify (Destructor Fst [] [Covar "a'])
     Destructor Fst [] (Covar "a")
     -}
    simplify (Destructor dt args coargs) = Destructor dt (simplify <$> args) (simplify <$> coargs)
