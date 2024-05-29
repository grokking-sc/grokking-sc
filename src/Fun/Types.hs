{- |
Module      : Fun.Types

Description : Type inference for the surface language Fun

This module implements a simple type inference algorithm for the surface
language Fun. The type inference algorithm follows the standard Hindley-Milner
approach, but we do not implement let-generalization.
-}
module Fun.Types (inferTypes) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS
import Data.List (find)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Fun.Syntax

-- | Type variables
type TyVar = Text

-- | Types are defined in section 4.1, Appendix A
data Ty
    = -- | Type variables
      TyVar TyVar
    | -- | The type of primitive integers
      IntTy
    | -- | The list type
      ListTy Ty
    | -- | The stream type
      StreamTy Ty
    | -- | The strict pair type
      PairTy Ty Ty
    | -- | The lazy pair type
      LPairTy Ty Ty
    | -- | The function type
      FunTy Ty Ty
    deriving (Show, Eq)

-- | Compute the set of free type variables of a type.
freeTyVars :: Ty -> S.Set TyVar
freeTyVars (TyVar v) = S.singleton v
freeTyVars IntTy = S.empty
freeTyVars (ListTy t) = freeTyVars t
freeTyVars (PairTy t1 t2) = S.union (freeTyVars t1) (freeTyVars t2)
freeTyVars (StreamTy t) = freeTyVars t
freeTyVars (LPairTy t1 t2) = S.union (freeTyVars t1) (freeTyVars t2)
freeTyVars (FunTy t1 t2) = S.union (freeTyVars t1) (freeTyVars t2)

-- | A typeclass for applying type substitutions.
class Zonk a where
    zonk :: M.Map TyVar Ty -> a -> a

instance Zonk Ty where
    zonk m (TyVar a) = case M.lookup a m of
        Nothing -> TyVar a
        Just ty -> ty
    zonk _ IntTy = IntTy
    zonk m (ListTy t) = ListTy (zonk m t)
    zonk m (PairTy t1 t2) = PairTy (zonk m t1) (zonk m t2)
    zonk m (StreamTy t) = StreamTy (zonk m t)
    zonk m (LPairTy t1 t2) = LPairTy (zonk m t1) (zonk m t2)
    zonk m (FunTy t1 t2) = FunTy (zonk m t1) (zonk m t2)

instance Zonk (Def Ty) where
    zonk m (Def name args cv body ret) =
        Def name ((\(x, t) -> (x, zonk m t)) <$> args) cv body (zonk m ret)

instance Zonk (Program Ty) where
    zonk m (MkProg defs) = MkProg (zonk m <$> defs)

instance Zonk Constraint where
    zonk m (t1, t2) = (zonk m t1, zonk m t2)

-- | An equality constraint between two types.
type Constraint = (Ty, Ty)

type Error = String

-------------------------------------------------------------------------------
-- Constraint Generation
-------------------------------------------------------------------------------

-- the constraint generation environment (only read from)
-- contains a map of types vor (term) variables
-- a map of types for covariables (labels)
-- and a program (that is already typed)
type GenReader = (M.Map Var Ty, M.Map Covar Ty, Program Ty)

-- the constraint generation state (updated during generation)
-- contains a number, which starts at 0 and is increased whenever a new type variable is generated
-- and contains a list of generated constraints
type GenState = (Int, [Constraint])

-- the generator monad
-- uses the RWS and Except monad
-- RWS contains the environment GenReader,
-- an output to write to (), here nothing
-- and a state GenState
-- Using the transformer RWST it is combined with the Except monad to add errors
type GenM a = RWST GenReader () GenState (Except Error) a

-- | Add a variable binding to the typing context.
addVarBindings :: [(Var, Ty)] -> GenReader -> GenReader
addVarBindings xs (env, coenv, prog) = (M.union (M.fromList xs) env, coenv, prog)

-- | Add a covariable binding to the typing context.
addCovarBinding :: Covar -> Ty -> GenReader -> GenReader
addCovarBinding v ty (env, coenv, prog) = (env, M.insert v ty coenv, prog)

-- | Generate a fresh type variable @ai@.
freshVar :: GenM Ty
freshVar = do
    (i, _) <- get
    modify (\(j, cs) -> (j + 1, cs))
    pure (TyVar (T.pack ("a" <> show i)))

-- | Add a constraint to the monad state.
addConstraint :: Constraint -> GenM ()
addConstraint c = modify (\(i, cs) -> (i, c : cs))

-- looks up a type definition in the environment
-- when the name is not contained in the environment proram an error is thrown
-- otherwise a list of argument types and the return type is returned
lookupDefinition :: Name -> GenM ([Ty], Ty)
lookupDefinition nm = do
    (_, _, MkProg prog) <- ask
    case find (\(Def nm' _ _ _ _) -> nm == nm') prog of
        Nothing -> throwError ("A toplevel function " <> T.unpack nm <> " is not contained in the program.")
        Just (Def _ args _ _ ret) -> pure (snd <$> args, ret)

-- | Generate constraints for a term and return its type.
genConstraintsTm :: Term -> GenM Ty
genConstraintsTm (VarT x) = do
    --
    --  x : τ ∈ Γ
    -- ―――――――――――― Var
    --  Γ ⊢ x : τ
    --
    (env, _, _) <- ask
    case M.lookup x env of
        Nothing -> throwError ("Variable " <> show x <> " not bound in environment.")
        Just tau -> pure tau
genConstraintsTm (Lit _) = do
    --
    -- ――――――――――――――― Lit
    --  Γ ⊢ ⌜n⌝ : Int
    --
    pure IntTy
genConstraintsTm (Op t1 _ t2) = do
    --
    --  Γ ⊢ t1 : Int    Γ ⊢ t2 : Int
    -- ―――――――――――――――――――――――――――――――― Op
    --        Γ ⊢ t1 ☉ t2 : Int
    --
    ty1 <- genConstraintsTm t1
    addConstraint (ty1, IntTy)
    ty2 <- genConstraintsTm t2
    addConstraint (ty2, IntTy)
    pure IntTy
genConstraintsTm (IfZ n t1 t2) = do
    --
    --  Γ ⊢ n : Int   Γ ⊢ t1 : τ   Γ ⊢ t2 : τ
    -- ―――――――――――――――――――――――――――――――――――――――――― Ifz
    --         Γ ⊢ ifz(n,t1,t2) : τ
    --
    ty <- genConstraintsTm n
    addConstraint (ty, IntTy)
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    addConstraint (ty1, ty2)
    pure ty1
genConstraintsTm (Let v t1 t2) = do
    --
    --  Γ ⊢ t1 : τ1   Γ, x : τ1 ⊢ t2 : τ2
    -- ――――――――――――――――――――――――――――――――――――― Let
    --      Γ ⊢ let x = t1 in t2 : τ2
    --
    ty1 <- genConstraintsTm t1
    local (addVarBindings [(v, ty1)]) $ genConstraintsTm t2
-- Toplevel Function Calls
-- Function calls without covariable argument
--
--   f(x_i:τ_i;a:τ'):σ ∈ P  Γ⊢t_i:τ_i  Γ⊢a:τ'
-- ――――――――――――――――――――――――――――――――――――――――― Call
--              Γ ⊢ f(t_i) : σ
--
--   a here is optional, when it is not present in P it is ignored
--
genConstraintsTm (Fun nm args Nothing) = do
    -- to type a toplevel call, its definition needs to be in the environment
    -- so first we look up the definition
    (argtys, retty) <- lookupDefinition nm
    -- the number of arguments of the call need to be the same as the number of defined arguments
    -- if they are not, throw an error
    if length args /= length argtys
        then
            throwError
                ( "Function "
                    <> T.unpack nm
                    <> " called with wrong number of arguments. Expected: "
                    <> show (length argtys)
                    <> " Got: "
                    <> show (length args)
                )
        else do
            -- generate constraints for all argument terms
            tys <- forM args genConstraintsTm
            -- the types of the arguments need to be the same as the ones defined in the definition
            -- thus we add constraints between them
            -- this always adds one constraint per argument, since we already checked the lengths of argtys and tys are equal
            forM_ (zip argtys tys) addConstraint
            -- the final type is then the looked up return type
            pure retty
-- function calls with covariable argument
genConstraintsTm (Fun nm args (Just cv)) = do
    -- again look up the definition
    (argtys, retty) <- lookupDefinition nm
    -- check argument lengths
    if length args /= length argtys
        then
            throwError
                ( "Function "
                    <> T.unpack nm
                    <> " called with wrong number of arguments. Expected: "
                    <> show (length argtys)
                    <> " Got: "
                    <> show (length args)
                )
        else do
            -- now we also need to find the type of the covariable
            (_, coenv, _) <- ask
            -- if it is in the environment, use that type
            -- otherwise create a new type variable
            cvTy <- case M.lookup cv coenv of Nothing -> freshVar; Just ty -> pure ty
            -- generate constraints for all arguments as before
            -- but now locally add the type of the covariable to the environment during constraint generation
            -- this ensures that whenever cv is used in an argument, it's type will be the same
            tys <- forM args (local (addCovarBinding cv cvTy) . genConstraintsTm)
            -- again add a constraint for each argument
            forM_ (zip argtys tys) addConstraint
            -- and return the defined return type
            pure retty
genConstraintsTm (Constructor Nil []) = do
    --
    -- ―――――――――――――――――――― Nil
    --  Γ ⊢ Nil : List(τ)
    --
    tau <- freshVar
    pure (ListTy tau)
genConstraintsTm (Constructor Cons [t1, t2]) = do
    --
    --  Γ ⊢ t1 : τ   Γ ⊢ t2 : List(τ)
    -- ―――――――――――――――――――――――――――――――― Cons
    --  Γ ⊢ Cons(t1,t2) : List(τ)
    --
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    addConstraint (ListTy ty1, ty2)
    pure ty2
genConstraintsTm (Constructor Tup [t1, t2]) = do
    --
    --  Γ ⊢ t1 : τ1   Γ ⊢ t2 : τ2
    -- ―――――――――――――――――――――――――――――― Tup
    --  Γ ⊢ Tup(t1,t2) : Pair(τ1,τ2)
    --
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    pure (PairTy ty1 ty2)
genConstraintsTm (Constructor ctor _) =
    throwError ("Constructor " <> show ctor <> " applied to wrong number of arguments.")
genConstraintsTm (Case t [MkClause Nil [] t_nil, MkClause Cons [x, xs] t_cons]) = do
    --
    --  Γ ⊢ t : List(σ)   Γ ⊢ t_nil : τ   Γ, x : σ, xs : List(σ) ⊢ t_cons : τ
    -- ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――― Case-List
    --  Γ ⊢ case t of { Nil => t_nil, Cons(x,xs) => t_cons } : τ
    --
    ty <- genConstraintsTm t
    a <- freshVar
    addConstraint (ty, ListTy a)
    retNil <- genConstraintsTm t_nil
    retCons <- local (addVarBindings [(x, a), (xs, ListTy a)]) (genConstraintsTm t_cons)
    addConstraint (retNil, retCons)
    pure retNil
genConstraintsTm (Case t [MkClause Tup [x, y] body]) = do
    --
    --  Γ ⊢ t : Pair(σ,σ')   Γ, x : σ, y : σ' ⊢ body : τ
    -- ――――――――――――――――――――――――――――――――――――――――――――――――――― Case-Pair
    --  Γ ⊢ case t of { Tup(x,y) => body } : τ
    --
    ty1 <- genConstraintsTm t
    a <- freshVar
    b <- freshVar
    addConstraint (ty1, PairTy a b)
    local (addVarBindings [(x, a), (y, b)]) (genConstraintsTm body)
genConstraintsTm tm@(Case _ _) = throwError ("Invalid case expression: " <> show tm)
genConstraintsTm (Destructor t Hd []) = do
    --
    --  Γ ⊢ t : Stream(τ)
    -- ――――――――――――――――――― Hd
    --  Γ ⊢ t.hd : τ
    --
    ty <- genConstraintsTm t
    a <- freshVar
    addConstraint (ty, StreamTy a)
    pure a
genConstraintsTm (Destructor t Tl []) = do
    --
    --   Γ ⊢ t : Stream(τ)
    -- ――――――――――――――――――――――― Tl
    --   Γ ⊢ t.tl : Stream(τ)
    --
    ty <- genConstraintsTm t
    a <- freshVar
    addConstraint (ty, StreamTy a)
    pure (StreamTy a)
genConstraintsTm (Destructor t Fst []) = do
    --
    --  Γ ⊢ t : LPair(τ,σ)
    -- ―――――――――――――――――――― Fst
    --  Γ ⊢ t.fst : τ
    --
    ty <- genConstraintsTm t
    a <- freshVar
    b <- freshVar
    addConstraint (ty, LPairTy a b)
    pure a
genConstraintsTm (Destructor t Snd []) = do
    --
    --  Γ ⊢ t : LPair(τ,σ)
    -- ―――――――――――――――――――――――― Snd
    --  Γ ⊢ t.snd : σ
    --
    ty <- genConstraintsTm t
    a <- freshVar
    b <- freshVar
    addConstraint (ty, LPairTy a b)
    pure b
genConstraintsTm (Destructor _ dtor _) =
    throwError ("Destructor " <> show dtor <> " called with wrong number of arguments")
genConstraintsTm (Cocase [MkClause Hd [] t1, MkClause Tl [] t2]) = do
    --
    --  Γ ⊢ t1 : τ   Γ ⊢ t2 : Stream(τ)
    -- ――――――――――――――――――――――――――――――――――――――――――――――― Stream
    --  Γ ⊢ cocase { hd => t1, tl => t2 } : Stream(τ)
    --
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    addConstraint (StreamTy ty1, ty2)
    pure (StreamTy ty1)
genConstraintsTm (Cocase [MkClause Fst [] t1, MkClause Snd [] t2]) = do
    --
    --  Γ ⊢ t1 : τ   Γ ⊢ t2 : σ
    -- ―――――――――――――――――――――――――――――――――――――――――――――――――― LPair
    --  Γ ⊢ cocase { fst => t1, snd => t2 } : LPair(τ,σ)
    --
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    pure (LPairTy ty1 ty2)
genConstraintsTm tm@(Cocase _) = throwError ("Invalid cocase expression: " <> show tm)
genConstraintsTm (Lam v t) = do
    --
    --  Γ, v : τ ⊢ t : σ
    -- ―――――――――――――――――――― Lam
    --  Γ ⊢ λv.t : τ -> σ
    --
    a <- freshVar
    ty <- local (addVarBindings [(v, a)]) $ genConstraintsTm t
    pure $ FunTy a ty
genConstraintsTm (App t1 t2) = do
    --
    --  Γ ⊢ t1 : τ -> σ   Γ ⊢ t2 : τ
    -- ――――――――――――――――――――――――――――――― App
    --      Γ ⊢ t1 t2 : σ
    --
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    ret <- freshVar
    addConstraint (ty1, FunTy ty2 ret)
    pure ret
genConstraintsTm (Goto t v) = do
    --
    --  Γ ⊢ t : τ     v : σ ∈ Γ
    -- ―――――――――――――――――――――――――― Goto
    --     Γ ⊢ Goto(t,v) : σ
    --
    ty1 <- genConstraintsTm t
    ty2 <- do
        (_, coenv, _) <- ask
        case M.lookup v coenv of
            Nothing -> throwError ("Covariable " <> show v <> " not bound in environment")
            Just ty -> pure ty
    addConstraint (ty1, ty2)
    freshVar
genConstraintsTm (Label v t) = do
    --
    --  Γ, v : τ ⊢ t : τ
    -- ―――――――――――――――――――――――― Label
    --  Γ ⊢ Label(v,t) : τ
    --
    a <- freshVar
    ty <- local (addCovarBinding v a) (genConstraintsTm t)
    addConstraint (a, ty)
    pure ty

-- to generate constraints for a definition (with already included types
-- we add the argument variables to the environment while generting constraints for the body
-- this already needs to have types included
-- these types are always type variables (see below), which are then resolved with the generated constraints
genConstraintsDef :: Def Ty -> GenM ()
genConstraintsDef (Def _ args _ body ret) = do
    ty <- local (addVarBindings args) $ genConstraintsTm body
    addConstraint (ty, ret)

-- | Annotate every toplevel definition with fresh typevariables for argument and return types.
annotateProgram :: Program () -> Program Ty
-- reverse is needed because annotateDefs reverses its argument list
annotateProgram (MkProg defs) = MkProg (reverse defs')
  where
    -- generates an infinite list of type variables
    -- all of the form bi with i some integer
    -- annotateDefs then takes all the ones needed for bodies and arguments and discards the rest
    defs' :: [Def Ty]
    defs' = annotateDefs defs [] [TyVar (T.pack ("b" <> show i)) | i <- [(0 :: Integer) ..]]

    -- takes unannotated definitions
    -- a list of already annotated  definitions
    -- a list of types to use for the definition
    -- this list is the infinite list of type variables generated above
    -- then returns a list of annotated definitions
    annotateDefs :: [Def ()] -> [Def Ty] -> [Ty] -> [Def Ty]
    -- once all definitions are annotated we are done
    annotateDefs [] acc _ = acc
    -- given at least one unannotated definition
    -- first annotate the arguments using annotateArgs
    -- this also returns a new list of types to annotate (so the same type variable is not used multiple times
    -- then add the next type variable for the return type
    -- add the newly annotated definition to the list of already annotated definitions
    -- recursively call annotateDefs for the remaining annotated variables
    -- make sure to remove the taken type vairables so they are not reused
    annotateDefs (Def nm args cv body () : rest) acc fvs = do
        let (args', fvs') = annotateArgs args fvs
        annotateDefs rest (Def nm args' cv body (head fvs') : acc) (tail fvs')

    -- given a list of argument variables and types for them
    -- returns a list of annotated variables and remaining types
    -- since the type list is inifinite this removes all used types and returns another infinite list of types
    -- zip only returns a tuple when both lists are nonempty
    -- thus the first element of the return tuple always has exactly (length args) elements
    -- and the second one is the original type list with (length args) elements removed
    annotateArgs :: [(Var, ())] -> [Ty] -> ([(Var, Ty)], [Ty])
    annotateArgs args fvs = (zip (fst <$> args) fvs, drop (length args) fvs)

-- generate constraints for a program
-- runs the constraint generation monad and returns either an error string
-- or the annotated program with a list of generated constraints
-- the argument and return types of the included definitions will all be type variables (see above)
-- once the constraints are resolved (see below) these will be substtituted
generateConstraints :: Program () -> Either String (Program Ty, [Constraint])
generateConstraints prog = do
    -- first annotate the program
    -- then run the constraint generation monad
    -- return the results of running the monad
    let prog'@(MkProg defs) = annotateProgram prog
    let initialReader = (M.empty, M.empty, prog')
    let initialState = (0, [])
    let act :: GenM ()
        act = forM_ defs genConstraintsDef
    case runExcept (runRWST act initialReader initialState) of
        Left err -> Left err
        Right (_, (_, cs), _) -> pure (prog', cs)

-------------------------------------------------------------------------------
-- Constraint Solving
-------------------------------------------------------------------------------

-- solve constraints takes a list of constraints and either returns an error or returns a map
-- this map maps type variables to types and is used to substitute type variables
-- it suns the solving monad below, the results are the results of that monad
solveConstraints :: [Constraint] -> Either Error (M.Map TyVar Ty)
solveConstraints cs = do
    let initial = MkSolverState cs M.empty
    case runExcept (runRWST run () initial) of
        Left err -> Left err
        Right ((), s, ()) -> Right (subst s)

-- state for the solver monad
-- contains constraints that have not been resolved yet
-- and a map of typevariables to types, which contains the already resolved variables
data SolverState = MkSolverState {todo :: [Constraint], subst :: M.Map TyVar Ty}

-- the solver monad
-- again uses the RWST monad transformer with the Except monad
-- now the environment and writer are both empty and the state is the solverstate
type SolverM a = RWST () () SolverState (Except Error) a

-- adds a new type variable to the state (the typevar map)
-- before adding the new substitution, the substitution is performed on the remaining constraints
performSubst :: TyVar -> Ty -> SolverM ()
performSubst a ty = do
    -- generate a new variable map containing only the given variable and type
    let m = M.fromList [(a, ty)]
    -- get the current map and remaining constraints
    MkSolverState todo subst <- get
    -- replace the given type variable in all remaining constraints
    -- this ensures a resolved variable does not appear again
    let todo' = zonk m <$> todo
    -- add the new substitution to the substitution map
    let subst' = fmap (zonk m) subst
    let subst'' = M.insert a ty subst'
    -- save the new substitution map and remaining constraints in the state
    put (MkSolverState todo' subst'')

-- | Add new constraints to the state of the solver.
addConstraints :: [Constraint] -> SolverM ()
addConstraints cs = modify (\(MkSolverState todos m) -> MkSolverState (cs ++ todos) m)

-- | Solve constraints until the todo list is empty.
run :: SolverM ()
run = do
    -- get reaming constraints
    todo <- gets todo
    case todo of
        -- once no more constraints are left we are done
        [] -> pure ()
        -- otherwise solve the current constraint and remove it from the remaining constraints
        -- then recursively call the run function again
        (c : _) -> do
            modify (\(MkSolverState todos m) -> MkSolverState (tail todos) m)
            solveConstraint c
            run

-- | Solve a single equality constraint.
solveConstraint :: Constraint -> SolverM ()
-- A constraint between a type variable and itself can be discarded.
solveConstraint (TyVar a, TyVar b) | a == b = pure ()
-- a constraint between a type var and another type
-- check if the typevar appears free in the other type
-- if it does this is a recursive type, which is not supported
-- thus an error is thrown
-- otherwise, the variable can be substituted by the given type in all constraints and added to the variable map
solveConstraint (TyVar a, ty)
    | a `S.member` freeTyVars ty = throwError ("Occurs check! " <> show a <> " occurs in " <> show ty)
    | otherwise = performSubst a ty
solveConstraint (ty, TyVar a)
    | a `S.member` freeTyVars ty = throwError ("Occurs check! " <> show a <> " occurs in " <> show ty)
    | otherwise = performSubst a ty
-- two int types are always equal
solveConstraint (IntTy, IntTy) = pure ()
-- For constraints between two lists, streams, functions, strict pairs and lazy pairs we
-- generate new subconstraints.
solveConstraint (ListTy t1, ListTy t2) = addConstraints [(t1, t2)]
solveConstraint (PairTy t1 t2, PairTy t1' t2') = addConstraints [(t1, t1'), (t2, t2')]
solveConstraint (StreamTy t1, StreamTy t2) = addConstraints [(t1, t2)]
solveConstraint (LPairTy t1 t2, LPairTy t1' t2') = addConstraints [(t1, t1'), (t2, t2')]
solveConstraint (FunTy t1 t2, FunTy t1' t2') = addConstraints [(t1, t1'), (t2, t2')]
-- In any other case we cannot unify the two types and emit an error.
solveConstraint (ty1, ty2) = throwError ("Cannot unify types: " <> show ty1 <> "and" <> show ty2)

-------------------------------------------------------------------------------
-- Type Inference
-------------------------------------------------------------------------------

-- | Infer the types of a program.
inferTypes :: Program () -> Either Error (Program Ty)
inferTypes prog = do
    -- Generate the constraints and annotate the toplevel definitions
    -- with unification variables.
    (prog', constraints) <- generateConstraints prog
    -- Solve the constraints and generate a unifier which maps type variables
    -- to types.
    subst <- solveConstraints constraints
    -- Apply the unifier to the program annotated with type variables.
    pure (zonk subst prog')
