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
    freshCovar,

    -- * free (Co-) Variables
    FreeV,
    Free(..),
    freeVars,
    freeCovars,
) where

import Core.Syntax
import Data.List (find)
import Data.Set qualified as S
import Data.Text qualified as T

{- | generate an infinite list of variables not occurring free in the argument list
This arguments need to have the FreeV class implemented
All generated variables will have the form xi with i starting from 0
-}

{-
 >>> freshVars []
 ["x0","x1",...]
 -}
freshVars :: (FreeV a) => [a] -> [Var]
freshVars xs = filter (not . \x -> x `elem` fvs) [T.pack ("x" <> show i) | i <- [(0 :: Integer) ..]]
  where
    fvs = freeVars xs

{- | generate a fresh variable not occuring free in the argument list
The arguments needs to have the FreeV class implemented
-}

{-
 >>> freshVar Done
 "x0"
 >>> freshVar (Var "x0")
 "x1"
-}
freshVar :: (FreeV a) => [a] -> Var
freshVar = head . freshVars

{- | generate fresh covariables not occuring in the argument list
this arguments need to have FreeV implemented
generated covariables will have the form ai with i starting at 0{
-}

{-
 >>> freshCovars []
 ["a0","a1",...]
-}
freshCovars :: (FreeV a) => [a] -> [Covar]
freshCovars xs = filter (not . \x -> x `elem` fcvs) [T.pack ("a" <> show i) | i <- [(0 :: Integer) ..]]
  where
    fcvs = freeCovars xs

{- | generate a fresh covariable not occurring in the argument list
the arguments need to have FreeV implemented
-}

{-
 >>> freshCovar Done
 "a0"
 >>> freshCovar (Covar "a0")
 "a1"
 -}
freshCovar :: (FreeV a) => [a] -> Covar
freshCovar = head . freshCovars

{- | Wrapper type to use with freshVar and freshCovar
Combines multiple different types all having FreeV implemented
For example, (MkFree Done) and (MkFree (Var "x")) are both of type Free
-}
data Free = forall a. (FreeV a) => MkFree a

instance FreeV Free where
    freeVars (MkFree x) = freeVars x
    freeCovars (MkFree x) = freeCovars x

-- | Type class for types containing free variables and covariables
class FreeV a where
    -- | compute all free variables
    freeVars :: a -> S.Set Var

    -- | compute all free covariables
    freeCovars :: a -> S.Set Covar

instance (FreeV a) => FreeV [a] where
    freeVars xs = S.unions (freeVars <$> xs)
    freeCovars xs = S.unions (freeCovars <$> xs)

instance FreeV Producer where
    {-
     >>> freeVars (Var "x")
     singleton "x"
     -}
    freeVars (Var v) = S.singleton v
    {-
     >>> freeVars (Lit 1)
     empty
     -}
    freeVars (Lit _) = S.empty
    -- mu binds a covariable, so this can be ignored
    {-
     >> freeVars (Mu "a" Done)
     empty
     -}
    freeVars (Mu _ s) = freeVars s
    -- combine free vars of all arguments
    {-
     >>> freeVars (Constructor Cons [Var "x", Nil])
     singleton "x"
     -}
    freeVars (Constructor _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)
    -- combine all free vars of the patterns
    {-
     >>> freeVars (Cocase [MkPattern Ap ["x"] ["a"] (Cut (Var "x") (Covar "a")))
     empty
     -}
    freeVars (Cocase pts) = freeVars pts

    -- a free variable has no covariables
    {-
     >>> freeCovars (Var "x")
     empty
     -}
    freeCovars (Var _) = S.empty
    -- a literal has no covariables
    {-
     >>> freeCovars (Lit 1)
     empty
     -}
    freeCovars (Lit _) = S.empty
    -- remove the bound covariable from the free covars of the bound statement
    -- mu is a binding occurrence of cv
    {-
     >>> freeCovars (Mu "a" (Cut (Var "x") (Covar "a")))
     empty
     -}
    freeCovars (Mu cv st) = S.delete cv (freeCovars st)
    {-
     >>> freeCovars (Constructor Nil [] [])
     empty
     -}
    freeCovars (Constructor _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)
    {-
     >>> freeCovars (Cocase (MkPattern Fst [] ["a"] (Cut (Var "x") (Covar "a"))))
     empty
     -}
    freeCovars (Cocase pts) = freeCovars pts

instance FreeV Consumer where
    {-
     >>> freeVars (Covar "a")
     empty
     -}
    freeVars (Covar _) = S.empty
    -- mu tilde binds v, so v is removed from the free vars of st
    {-
     >>> freeVars (MuTilde "x" (Cut (Var "x") (Covar "a")))
     empty
     -}
    freeVars (MuTilde v st) = S.delete v (freeVars st)
    {-
     >>> freeVars (Case (MkPattern Nil [] []))
     empty
    -}
    freeVars (Case pts) = freeVars pts
    {-
     >>> freeVars (Destructor Fst [] [Covar "a"])
     empty
     -}
    freeVars (Destructor _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)

    {-
     >>> freeCovars (Covar "a")
     singleton "a"
     -}
    freeCovars (Covar cv) = S.singleton cv
    -- mutilde binds a variable, so this variable can be ignored
    {-
     >>> freeCovars (Mutilde "x" (Cut (Var "x") (Covar "a")))
     singleton "a"
     -}
    freeCovars (MuTilde _ st) = freeCovars st
    -- same as for the analogous producers
    {-
     >>> freeCovars (Case [])
     empty
     -}
    freeCovars (Case pts) = freeCovars pts
    {-
     freeCovars (Destructor Fst [] [Covar "a"]
     singleton "a"
     -}
    freeCovars (Destructor _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)

instance FreeV Statement where
    {-
     >>> freeVars (Cut (Var "x") (Covar "a"))
     singleton "x"
    -}
    freeVars (Cut p c) = S.union (freeVars p) (freeVars c)
    {-
     >>> freeVars (Op (Lit 1) Sum (Var "x") (Covar "a"))
     singleton "x"
     -}
    freeVars (Op p1 _ p2 c) = S.unions [freeVars p1, freeVars p2, freeVars c]
    {-
     >>> freeVars (IfZ (Lit 1) Done Done
     empty
     -}
    freeVars (IfZ p s1 s2) = S.unions [freeVars p, freeVars s1, freeVars s2]
    {-
     >>> freeVars (Fun "Exit" [] ["a"])
     empty
     -}
    freeVars (Fun _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)
    {-
     >>> freeVars Done
     empty
    -}
    freeVars Done = S.empty

    {-
     >>> freeCovars (Cut (Var "x") (Covar "a"))
     singleton "a"
    -}
    freeCovars (Cut p c) = S.union (freeCovars p) (freeCovars c)
    {-
     >>> freeCovars (Op (Lit 1) Sum (Var "x") (Covar "a"))
     singleton "a"
     -}
    freeCovars (Op p1 _ p2 c) = S.unions [freeCovars p1, freeCovars p2, freeCovars c]
    {-
     freeCovars (IfZ (Lit 1) Done Done
     empty
     -}
    freeCovars (IfZ p s1 s2) = S.unions [freeCovars p, freeCovars s1, freeCovars s2]
    {-
     >>> freeCovars (Fun "Exit" [] ["a"])
     empty
     -}
    freeCovars (Fun _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)
    {-
     >>> freeCovars Done
     empty
    -}
    freeCovars Done = S.empty

instance FreeV (Pattern a) where
    -- free variables/covariables of a pattern are the free ones of the bound statement
    -- but the ones bound in the pattern have to be removed
    {-
     >>> freeVars (MkPattern (Cons ["x","xs"]) (Cut (Var "x") (Covar "a"))
     empty
     -}
    freeVars MkPattern{xtor = _, patv = vars, patcv = _, patst = st} = S.difference (freeVars st) (S.fromList vars)

    {-
     >>> freeCovars (MkPattern (Cons ["x","xs"]) (Cut (Var "x") (Covar "a"))
     singleton "a"
    -}
    freeCovars MkPattern{xtor = _, patv = _, patcv = cvars, patst = st} = S.difference (freeCovars st) (S.fromList cvars)

--- Substitution

-- | Type class for substituting all variables and covariables in an expression
class Subst a where
    -- | substitute each variable and each covariable in some expression
    -- the first two arguments are the replacements to be done
    -- each tuple (prd,var) replaces var by prd and analogous for tuples (cns,covar)
    -- the third argument is where the subtitution will be performed
    substSim :: [(Producer, Var)] -> [(Consumer, Covar)] -> a -> a

    -- | substitute a single variable by a producer in some expression
    substVar :: Producer -> Var -> a -> a
    substVar p v = substSim [(p, v)] []

    -- | substitute a single covariable by a consumer in some expression
    substCovar :: Consumer -> Covar -> a -> a
    substCovar c v = substSim [] [(c, v)]

instance (Subst a) => Subst [a] where
    substSim ps cs xs = substSim ps cs <$> xs

{- | Substitution instance for patterns
this might need alpha renaming to ensure bound (co-) variables of a pattern are not substituted
-}
instance (Subst (Pattern a)) where
    -- to substitute within a pattern, bound variables and covariables need to be ignored
    -- vars/covars to be substituted might be bound by the pattern
    -- thus substitution might require alpha renaming
    {-
     >>> substSim [(Var "x","y")] [] (MkPattern Cons ["x","xs"] (Cut (Var "x") (Covar "a")))
     MkPattern Cons ["x0","x1"] [] (Cut (Var "x0") (Covar "a"))
     -}
    substSim ps cs (MkPattern xt vars cvars st) = do
        -- combine everything whose free variables we need
        -- this includes
        -- the bound statement st
        -- variables to be substituted (fst <$> ps)
        -- producers to substitute for the variables (snd<$>ps)
        -- covariables to be substituted (fst<$>cs)
        -- consumers to substitute for the covariables (snd<$>cs)
        let foo :: [Free] =
                MkFree st
                    : (MkFree . Var . snd <$> ps)
                        <> (MkFree . fst <$> ps)
                        <> (MkFree . Covar . snd <$> cs)
                        <> (MkFree . fst <$> cs)
        -- generate fresh variables and covariables for the pattern
        -- these replace the ones bound in the pattern
        -- the number of vars and covars will be the same
        -- and their names will not clash with any of the ones in foo
        -- thus these are all fresh wrt the entire pattern
        let freshvars = take (length vars) (freshVars foo)
        let freshcovars = take (length cvars) (freshCovars foo)
        -- substitute the original variables and covariables by the newly generated ones
        -- this ensures there is no shadowing with the variables/covariables that are to be substituted
        let st' = substSim (zip (Var <$> freshvars) vars) (zip (Covar <$> freshcovars) cvars) st
        -- return the new pattern with renamed bound variables
        -- and substitute the variables/covariables to actually be substituted in the new statement
        -- since st' does not contain any of the originally bound variables of the pattern, no substitutions fail due to shadowing
        MkPattern xt freshvars freshcovars (substSim ps cs st')

instance Subst Producer where
    -- to substitute a variable we check if its contained in the given substitution list
    -- if so return the new one, otherwise return the original one
    -- covariables can be ignored here, since variables cannot contain covariables
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
    -- as with patterns, mu abstractions require alpha renaming to avoid shadowing
    {-
     >>> substSym [Lit 1,"x"] [Covar "b","a"] (Mu "a" (Cut (Var "x") (Covar "a")))
     Mu "a0" (Cut (Lit 1) (Covar "a0")
     -}
    substSim ps cs (Mu cv st) = do
        -- we replace the original covariable cv with a new one cv'
        -- this new covariable will not appear free in any of the terms in the list
        -- this list contains
        -- the bound statement
        -- all covariables to be substituted (snd <$> cs)
        -- all consumers to be substituted (fst <$> cs)
        -- all producers to be substituted (fst <$> ps)
        let cv' =
                freshCovar
                    (MkFree st : (MkFree . Covar . snd <$> cs) ++ (MkFree . fst <$> cs) ++ (MkFree . fst <$> ps))
        -- we then need to substitute cv by cv', so the new variable is correctly bound by mu
        let st' = substCovar (Covar cv') cv st
        -- lastly, substitute ps and cs in the new statement no longer containing cv
        -- the new mu will have cv' bound instead of cv
        Mu cv' (substSim ps cs st')
    -- for constructors and cocases, substitution only needs to consider the arguments/patterns
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
    -- to substitute a covariable we look it up in the substitution list
    -- if it is found, return the new covariable, otherwise the original one
    -- variables are ignored here, since covariables do not contain variables
    {-
     >>> substSim [] (Covar "a","b") (Covar "b")
     Covar "a"

     >>> substSym [] (Covar "a", "b") (Covar "a")
     Covar "a"
     -}
    substSim _ cs (Covar v1) = case find (\(_, v) -> v == v1) cs of
        Nothing -> Covar v1
        Just (c, _) -> c
    -- mu tilde works analogously to mu, but with variables instead of covariables
    -- we alpha-rename the bound variable to ensure no shadowing
    {-
     >>> substSim [(Lit 1, "x")] [(Covar "b","a")] (Mutilde "x" (Cut (Var "x") (Covar "a")))
     MuTilde "x0" (Cut (Var "x0") (Covar "b"))
     -}
    substSim ps cs (MuTilde v st) = do
        -- generate a fresh variable not contained in the bound statement, any of the producers and consumers to be substituted,
        -- not equal to any variable that needs to be substituted and not equal to the originally bound variable
        let v' =
                freshVar (MkFree st : (MkFree . Var . snd <$> ps) ++ (MkFree . fst <$> ps) ++ (MkFree . fst <$> cs))
        -- replace the bound variable by the new one
        let st' = substVar (Var v') v st
        -- perform the substitution on the new statement
        -- and bind the new variable instead of the old one
        MuTilde v' (substSim ps cs st')
    -- cases and destructors are analogous to the corresponding producers
    -- substitute in the patterns or the arguments
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
    {-
     >>> substSym [] [] (Fun "Exit" [] [])
     Fun "Exit" [] []
    -}
    substSim ps cs (Fun nm pargs cargs) = Fun nm (substSim ps cs pargs) (substSim ps cs cargs)
    {-
     >>> substSim [] [] Done
     Done
    -}
    substSim _ _ Done = Done
