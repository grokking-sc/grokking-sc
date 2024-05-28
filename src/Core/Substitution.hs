{- |
Module      : Core.Substitution
Description : Substitution  for (co)variables.

This module contains functions for generating fresh variables
and covariables, and for substituting producers and consumers
for variables and covariables.
-}
module Core.Substitution where

import Core.Syntax
import Data.List (find)
import Data.Set qualified as S
import Data.Text qualified as T

-- fresh variables and covariables
-- freshv variables are all variables with names xi
-- all i where xi already appears free in xs is removed
freshVars :: (FreeV a) => [a] -> [Var]
freshVars xs = filter (not . \x -> x `elem` fvs) [T.pack ("x" <> show i) | i <- [(0 :: Integer) ..]]
  where
    fvs = freeVars xs

-- to get a single free variable, take the first one in the list
freshVar :: (FreeV a) => [a] -> Var
freshVar = head . freshVars

-- works the same as free variables,
-- now the prefix is a instead of x
freshCovars :: (FreeV a) => [a] -> [Covar]
freshCovars xs = filter (not . \x -> x `elem` fcvs) [T.pack ("a" <> show i) | i <- [(0 :: Integer) ..]]
  where
    fcvs = freeCovars xs

freshCovar :: (FreeV a) => [a] -> Covar
freshCovar = head . freshCovars

-- helper type for free variables/covariables
data Free = forall a. (FreeV a) => MkFree a

-- since all terms of type Free have a FreeV instance, we can always get free vars and covars
instance FreeV Free where
    freeVars (MkFree x) = freeVars x
    freeCovars (MkFree x) = freeCovars x

-- Free Variables and Covariables
class FreeV a where
    freeVars :: a -> S.Set Var
    freeCovars :: a -> S.Set Covar

-- to get free vars/covars of a list, map over that list
instance (FreeV a) => FreeV [a] where
    freeVars xs = S.unions (freeVars <$> xs)
    freeCovars xs = S.unions (freeCovars <$> xs)

instance FreeV Producer where
    -- a variable without surrounding binding is free
    freeVars (Var v) = S.singleton v
    -- literals have no free variables
    freeVars (Lit _) = S.empty
    -- mu binds a covariable, so this one can be ignored
    freeVars (Mu _ s) = freeVars s
    -- combine free vars of all arguments
    freeVars (Constructor _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)
    -- combine all free vars of the patterns
    freeVars (Cocase pts) = freeVars pts

    -- a free variable has no covariables
    freeCovars (Var _) = S.empty
    -- a literal has no covariables
    freeCovars (Lit _) = S.empty
    -- remove the bound covariable from the free covars of the bound statement
    -- mu is a binding occurrence of cv
    freeCovars (Mu cv st) = S.delete cv (freeCovars st)
    -- same as for vars
    freeCovars (Constructor _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)
    freeCovars (Cocase pts) = freeCovars pts

instance FreeV Consumer where
    -- a covar has no free variable
    freeVars (Covar _) = S.empty
    -- mu tilde binds v, so v is removed from the free vars of st
    freeVars (MuTilde v st) = S.delete v (freeVars st)
    -- same as for the analogous producers
    freeVars (Case pts) = freeVars pts
    freeVars (Destructor _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)

    -- covariable without binding is free
    freeCovars (Covar cv) = S.singleton cv
    -- mutilde binds a variable, so this variable can be ignored
    freeCovars (MuTilde _ st) = freeCovars st
    -- same as for the analogous producers
    freeCovars (Case pts) = freeCovars pts
    freeCovars (Destructor _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)

instance FreeV Statement where
    -- a cut does not bind variables, so combine the free variables of its producer and consumer term
    freeVars (Cut p c) = S.union (freeVars p) (freeVars c)
    -- same a for cuts
    freeVars (Op p1 _ p2 c) = S.unions [freeVars p1, freeVars p2, freeVars c]
    -- same as for cuts
    freeVars (IfZ p s1 s2) = S.unions [freeVars p, freeVars s1, freeVars s2]
    -- same as for cuts
    freeVars (Fun _ pArgs cArgs) = S.union (freeVars pArgs) (freeVars cArgs)
    freeVars Done = S.empty

    -- all the same as for variables
    freeCovars (Cut p c) = S.union (freeCovars p) (freeCovars c)
    freeCovars (Op p1 _ p2 c) = S.unions [freeCovars p1, freeCovars p2, freeCovars c]
    freeCovars (IfZ p s1 s2) = S.unions [freeCovars p, freeCovars s1, freeCovars s2]
    freeCovars (Fun _ pArgs cArgs) = S.union (freeCovars pArgs) (freeCovars cArgs)
    freeCovars Done = S.empty

instance FreeV (Pattern a) where
    -- free variables/covariables of a pattern are the free ones of the bound statement
    -- but the ones bound in the pattern have to be removed
    freeVars MkPattern{xtor = _, patv = vars, patcv = _, patst = st} = S.difference (freeVars st) (S.fromList vars)
    freeCovars MkPattern{xtor = _, patv = _, patcv = cvars, patst = st} = S.difference (freeCovars st) (S.fromList cvars)

--- Substitution
-- class for types containing variables and covariables that can be substituted
class Subst a where
    -- general substitution function, substitutes both variables and covariables
    -- each tuple (p,v) substitutes v for p whenever v appears free
    -- note that variables can only be substituted by producers and covariables by consumers
    -- only this function needs to be implemented, the others are implemented using this function
    substSim :: [(Producer, Var)] -> [(Consumer, Covar)] -> a -> a

    -- to substitute a single variable, use the general function with a singleton and empty list
    substVar :: Producer -> Var -> a -> a
    substVar p v = substSim [(p, v)] []

    -- analogous to variable substitution
    substCovar :: Consumer -> Covar -> a -> a
    substCovar c v = substSim [] [(c, v)]

-- same as with free vars
-- map over list
instance (Subst a) => Subst [a] where
    substSim ps cs xs = substSim ps cs <$> xs

instance (Subst (Pattern a)) where
    -- to substitute within a pattern, bound variables and covariables need to be ignored
    -- vars/covars to be substituted might be bound by the pattern
    -- thus substitution might require alpha renaming
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
    substSim ps _ (Var v1) = case find (\(_, v) -> v == v1) ps of
        Nothing -> Var v1
        Just (p, _) -> p
    -- literals cannot include any variables/covariables
    substSim _ _ (Lit n) = Lit n
    -- as with patterns, mu abstractions require alpha renaming to avoid shadowing
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
    substSim ps cs (Constructor ct pargs cargs) = Constructor ct (substSim ps cs pargs) (substSim ps cs cargs)
    substSim ps cs (Cocase patterns) = Cocase (substSim ps cs patterns)

instance Subst Consumer where
    -- to substitute a covariable we look it up in the substitution list
    -- if it is found, return the new covariable, otherwise the original one
    -- variables are ignored here, since covariables do not contain variables
    substSim _ cs (Covar v1) = case find (\(_, v) -> v == v1) cs of
        Nothing -> Covar v1
        Just (c, _) -> c
    -- mu tilde works analogously to mu, but with variables instead of covariables
    -- we alpha-rename the bound variable to ensure no shadowing
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
    substSim ps cs (Case patterns) = Case (substSim ps cs patterns)
    substSim ps cs (Destructor dt pargs cargs) = Destructor dt (substSim ps cs pargs) (substSim ps cs cargs)

instance Subst Statement where
    -- no statement binds any variable, so substitution is performed on the contained producers/consumers
    substSim ps cs (Cut p c) = Cut (substSim ps cs p) (substSim ps cs c)
    substSim ps cs (Op p1 op p2 c) = Op (substSim ps cs p1) op (substSim ps cs p2) (substSim ps cs c)
    substSim ps cs (IfZ p s1 s2) = IfZ (substSim ps cs p) (substSim ps cs s1) (substSim ps cs s2)
    substSim ps cs (Fun nm pargs cargs) = Fun nm (substSim ps cs pargs) (substSim ps cs cargs)
    substSim _ _ Done = Done
