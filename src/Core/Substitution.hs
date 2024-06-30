{- |
Module      : Core.Substitution
Description : Substitution  for (co)variables.

This module contains functions for generating fresh variables
and covariables, and for substituting producers and consumers
for variables and covariables.
-}
module Core.Substitution (
    -- * Substititions
    substVar,
    substCovar,
    substSim,
    Subst,

    -- * Fresh (Co-) Variables
    freshVar,
    freshVarFrom,
    freshCovar,
    freshCovarFrom,

    -- * free (Co-) Variables
    FreeV,
    Free (..),
    freeVars,
    freeCovars,
) where

import Core.Syntax
import Data.List (find)
import Data.Set qualified as S
import Data.Text qualified as T

{-
 >>> freshVarsFrom [] S.empty
 ["x0","x1",...]
 -}
freshVarsFrom :: (FreeV a) => [a] -> S.Set Var -> [Var]
freshVarsFrom xs s = filter (not . \x -> x `elem` fvs) [T.pack ("x" <> show i) | i <- [(0 :: Integer) ..]]
  where
    fvs = freeVars xs `S.union` s

{-
 >>> freshVars []
 ["x0","x1",...]
 -}
freshVars :: (FreeV a) => [a] -> [Var]
freshVars xs = freshVarsFrom xs S.empty

{-
 >>> freshVarFrom Done S.empty
 "x0"
 >>> freshVarFrom [Var (T.pack "x0")] (S.singleton (T.pack "x1"))
 "x2"
-}
freshVarFrom :: (FreeV a) => [a] -> S.Set Var -> Var
freshVarFrom xs s = head $ freshVarsFrom xs s

{-
 >>> freshVar Done
 "x0"
 >>> freshVar [Var (T.pack "x0")]
 "x1"
-}
freshVar :: (FreeV a) => [a] -> Var
freshVar = head . freshVars

{-
 >>> freshCovarsFrom [] S.empty
 ["a0","a1",...]
-}
freshCovarsFrom :: (FreeV a) => [a] -> S.Set Covar -> [Covar]
freshCovarsFrom xs s = filter (not . \x -> x `elem` fcvs) [T.pack ("a" <> show i) | i <- [(0 :: Integer) ..]]
  where
    fcvs = freeCovars xs `S.union` s

{-
 >>> freshCovars []
 ["a0","a1",...]
-}
freshCovars :: (FreeV a) => [a] -> [Covar]
freshCovars xs = freshCovarsFrom xs S.empty

{-
 >>> freshCovarFrom Done S.empty
 "a0"
 >>> freshCovarFrom [Covar (T.pack "a0")] (S.singleton (T.pack "a1"))
 "a2"
 -}
freshCovarFrom :: (FreeV a) => [a] -> S.Set Covar -> Covar
freshCovarFrom xs s = head $ freshCovarsFrom xs s

{-
 >>> freshCovar Done
 "a0"
 >>> freshCovar [Covar (T.pack "a0")]
 "a1"
 -}
freshCovar :: (FreeV a) => [a] -> Covar
freshCovar = head . freshCovars

{- | Existential wrapper around the FreeV typeclass.
This allows to simultaneously compute free variables for terms from different
syntactic categories.
-}
data Free = forall a. (FreeV a) => MkFree a

instance FreeV Free where
    freeVars (MkFree x) = freeVars x
    freeCovars (MkFree x) = freeCovars x

-- | Compute free variables and covariables
class FreeV a where
    -- | Compute all free variables.
    freeVars :: a -> S.Set Var

    -- | Compute all free covariables.
    freeCovars :: a -> S.Set Covar

instance (FreeV a) => FreeV [a] where
    freeVars xs = S.unions (freeVars <$> xs)
    freeCovars xs = S.unions (freeCovars <$> xs)

instance FreeV Producer where
    freeVars (Var v) = S.singleton v
    freeVars (Lit _) = S.empty
    -- mu binds a covariable, so this can be ignored
    freeVars (Mu _ s) = freeVars s
    freeVars (Constructor _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)
    freeVars (Cocase pts) = freeVars pts

    freeCovars (Var _) = S.empty
    freeCovars (Lit _) = S.empty
    freeCovars (Mu cv st) = S.delete cv (freeCovars st)
    freeCovars (Constructor _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)
    freeCovars (Cocase pts) = freeCovars pts

instance FreeV Consumer where
    freeVars (Covar _) = S.empty
    freeVars (MuTilde v st) = S.delete v (freeVars st)
    freeVars (Case pts) = freeVars pts
    freeVars (Destructor _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)

    freeCovars (Covar cv) = S.singleton cv
    -- mutilde binds a variable, so this variable can be ignored
    freeCovars (MuTilde _ st) = freeCovars st
    freeCovars (Case pts) = freeCovars pts
    freeCovars (Destructor _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)

instance FreeV Statement where
    freeVars (Cut p c) = S.union (freeVars p) (freeVars c)
    freeVars (Op p1 _ p2 c) = S.unions [freeVars p1, freeVars p2, freeVars c]
    freeVars (IfZ p s1 s2) = S.unions [freeVars p, freeVars s1, freeVars s2]
    freeVars (Fun _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)
    freeVars Done = S.empty

    freeCovars (Cut p c) = S.union (freeCovars p) (freeCovars c)
    freeCovars (Op p1 _ p2 c) = S.unions [freeCovars p1, freeCovars p2, freeCovars c]
    freeCovars (IfZ p s1 s2) = S.unions [freeCovars p, freeCovars s1, freeCovars s2]
    freeCovars (Fun _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)
    freeCovars Done = S.empty

instance FreeV (Pattern a) where
    freeVars MkPattern{xtor = _, patv = vars, patcv = _, patst = st} = S.difference (freeVars st) (S.fromList vars)

    freeCovars MkPattern{xtor = _, patv = _, patcv = cvars, patst = st} = S.difference (freeCovars st) (S.fromList cvars)

--- Substitution

{- | Type class which allows for the simultaneous substitution of
producers for variables and consumers for covariables
-}
class Subst a where
    -- | Simultaneous substitution
    substSim :: [(Producer, Var)] -> [(Consumer, Covar)] -> a -> a

    -- | Substitute a single variable by a producer
    substVar :: Producer -> Var -> a -> a
    substVar p v = substSim [(p, v)] []

    -- | Substitute a single covariable by a consumer
    substCovar :: Consumer -> Covar -> a -> a
    substCovar c v = substSim [] [(c, v)]

instance (Subst a) => Subst [a] where
    substSim ps cs xs = substSim ps cs <$> xs

instance (Subst (Pattern a)) where
    {-
     >>> substSim [(Var "x","y")] [] (MkPattern Cons ["x","xs"] (Cut (Var "x") (Covar "a")))
     MkPattern Cons ["x0","x1"] [] (Cut (Var "x0") (Covar "a"))
     -}
    substSim ps cs (MkPattern xt vars cvars st) = do
        let foo :: [Free] =
                MkFree st
                    : (MkFree . Var . snd <$> ps)
                        <> (MkFree . fst <$> ps)
                        <> (MkFree . Covar . snd <$> cs)
                        <> (MkFree . fst <$> cs)
        let freshvars = take (length vars) (freshVars foo)
        let freshcovars = take (length cvars) (freshCovars foo)
        let st' = substSim (zip (Var <$> freshvars) vars) (zip (Covar <$> freshcovars) cvars) st
        MkPattern xt freshvars freshcovars (substSim ps cs st')

instance Subst Producer where
    {-
     >>> substSim [(Var "x","y")] [] (Var "y")
     Var "x"
     >>> substSym [(Var "y","z")] [] (Var "x")
     Var "x"
    -}
    substSim ps _ (Var v1) = case find (\(_, v) -> v == v1) ps of
        Nothing -> Var v1
        Just (p, _) -> p
    {-
     >>> substSym [Var "x","y"] [Covar "a","b"] (Lit n)
     Lit n
    -}
    substSim _ _ (Lit n) = Lit n
    {-
     >>> substSym [Lit 1,"x"] [Covar "b","a"] (Mu "a" (Cut (Var "x") (Covar "a")))
     Mu "a0" (Cut (Lit 1) (Covar "a0")
     -}
    substSim ps cs (Mu cv st) = do
        let cv' =
                freshCovar
                    (MkFree st : (MkFree . Covar . snd <$> cs) ++ (MkFree . fst <$> cs) ++ (MkFree . fst <$> ps))
        let st' = substCovar (Covar cv') cv st
        Mu cv' (substSim ps cs st')
    {-
     >>> substSim [] [] (Constructor Nil [] [])
     Constructor Nil [] []
     -}
    substSim ps cs (Constructor ct pargs cargs) = Constructor ct (substSim ps cs pargs) (substSim ps cs cargs)
    {-
     >>> substSim [] [] (Cocase (MkPattern Fst [] ["a"] Done)
     Cocase (MkPattern Fst [] ["a"] Done)
     -}
    substSim ps cs (Cocase patterns) = Cocase (substSim ps cs patterns)

instance Subst Consumer where
    {-
     >>> substSim [] (Covar "a","b") (Covar "b")
     Covar "a"

     >>> substSym [] (Covar "a", "b") (Covar "a")
     Covar "a"
     -}
    substSim _ cs (Covar v1) = case find (\(_, v) -> v == v1) cs of
        Nothing -> Covar v1
        Just (c, _) -> c
    {-
     >>> substSim [(Lit 1, "x")] [(Covar "b","a")] (Mutilde "x" (Cut (Var "x") (Covar "a")))
     MuTilde "x0" (Cut (Var "x0") (Covar "b"))
     -}
    substSim ps cs (MuTilde v st) = do
        let v' =
                freshVar (MkFree st : (MkFree . Var . snd <$> ps) ++ (MkFree . fst <$> ps) ++ (MkFree . fst <$> cs))
        let st' = substVar (Var v') v st
        MuTilde v' (substSim ps cs st')
    {-
     >>> substSym [] [] (Case (MkPattern Nil [] []))
     Case (MkPattern Nil [] [])
     -}
    substSim ps cs (Case patterns) = Case (substSim ps cs patterns)
    {-
     >>> substSim [] [(Covar "b","a")] (Destructor Fst [] [Covar "a"])
     Destructor Fst [] [Covar "b"]
     -}
    substSim ps cs (Destructor dt pargs cargs) = Destructor dt (substSim ps cs pargs) (substSim ps cs cargs)

instance Subst Statement where
    {-
     >>> substSim [(Lit 1,"x")] [(Covar "b","a")] (Cut (Var "x") (Covar "a"))
     Cut (Lit 1) (Covar "b")
     -}
    substSim ps cs (Cut p c) = Cut (substSim ps cs p) (substSim ps cs c)
    {-
     >>> substSim [(Lit 2,"x")] [] (Op (Lit 1) Sum (Var "x") (Covar "a")
     Op (Lit 1) Sum (Lit 2) (Covar "a")
     -}
    substSim ps cs (Op p1 op p2 c) = Op (substSim ps cs p1) op (substSim ps cs p2) (substSim ps cs c)
    {-
     >>> substSim [(Lit 1, "x")] [] (IfZ (Var "x") Done Done)
     IfZ (Lit 1) Done DOne
     -}
    substSim ps cs (IfZ p s1 s2) = IfZ (substSim ps cs p) (substSim ps cs s1) (substSim ps cs s2)
    substSim ps cs (Fun nm pargs cargs) = Fun nm (substSim ps cs pargs) (substSim ps cs cargs)
    substSim _ _ Done = Done
