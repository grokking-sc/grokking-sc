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
    zonk m (Def name args cvs body ret) =
        Def
            name
            ((\(x, t) -> (x, zonk m t)) <$> args)
            ((\(x, t) -> (x, zonk m t)) <$> cvs)
            body
            (zonk m ret)

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

{- | The constraint generation environment
maps locally bound variables and covariables to their types.
It also contains an environment of typed top-level definitions.
-}
type GenReader = (M.Map Var Ty, M.Map Covar Ty, Program Ty)

{- | The constraint generation state contains a number which is used
to generate fresh unification variables and a list of generated
constraints.
-}
type GenState = (Int, [Constraint])

{- | The GenM Monad stack contains:
- A reader component of type @GenReader@
- A state component of type @GenState@
- An error component of type @Error@
-}
type GenM a = RWST GenReader () GenState (Except Error) a

-- | Add variable bindings to the typing context.
addVarBindings :: [(Var, Ty)] -> GenReader -> GenReader
addVarBindings xs (env, coenv, prog) = (M.union (M.fromList xs) env, coenv, prog)

-- | Add covariable bindings to the typing context.
addCovarBindings :: [(Covar, Ty)] -> GenReader -> GenReader
addCovarBindings xs (env, coenv, prog) = (env, M.union (M.fromList xs) coenv, prog)

-- | Generate a fresh unification variable @ai@.
freshVar :: GenM Ty
freshVar = do
    (i, _) <- get
    modify (\(j, cs) -> (j + 1, cs))
    pure (TyVar (T.pack ("a" <> show i)))

-- | Add a constraint.
addConstraint :: Constraint -> GenM ()
addConstraint c = modify (\(i, cs) -> (i, c : cs))

-- |  Look up the type signature of a function declared in the program.
lookupDefinition :: Name -> GenM ([Ty], [Ty], Ty)
lookupDefinition nm = do
    (_, _, MkProg prog) <- ask
    case find (\(Def nm' _ _ _ _) -> nm == nm') prog of
        Nothing -> throwError ("A top-level function " <> T.unpack nm <> " is not contained in the program.")
        Just (Def _ args cvs _ ret) -> pure (snd <$> args, snd <$> cvs, ret)

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
--
--   f(x_i:τ_i;a_j:τ'_j):σ ∈ P  Γ⊢t_i:τ_i  Γ⊢a_j:τ'_j
-- ――――――――――――――――――――----------――――――――――――――――――――― Call
--              Γ ⊢ f(t_i;a_j) : σ
--
genConstraintsTm (Fun nm args cvs) = do
    (argtys, cvtys, retty) <- lookupDefinition nm
    if length args /= length argtys || length cvs /= length cvtys
        then
            throwError
                ( "Function "
                    <> T.unpack nm
                    <> " called with wrong number of arguments. Expected: "
                    <> show (length argtys)
                    <> " + "
                    <> show (length cvtys)
                    <> " Got: "
                    <> show (length args)
                    <> " + "
                    <> show (length cvs)
                )
        else do
            (_, coenv, _) <- ask
            ptys <- forM args genConstraintsTm
            forM_ (zip argtys ptys) addConstraint
            ctys <-
                forM
                    cvs
                    ( \cv -> case M.lookup cv coenv of
                        Nothing -> throwError ("Variable " <> show cv <> " not bound in environment.")
                        Just tau -> pure tau
                    )
            forM_ (zip cvtys ctys) addConstraint
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
    --  Γ ⊢ t : τ     v : τ ∈ Γ
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
    ty <- local (addCovarBindings [(v, a)]) (genConstraintsTm t)
    addConstraint (a, ty)
    pure ty

-- | Generate constraints for a top-level definition.
genConstraintsDef :: Def Ty -> GenM ()
genConstraintsDef (Def _ args cvs body ret) = do
    ty' <- local (addCovarBindings cvs . addVarBindings args) $ (genConstraintsTm body)
    addConstraint (ty', ret)

{- | Annotate every top-level definition with fresh unification variables @bi@
for argument and return types.
-}
annotateProgram :: Program () -> Program Ty
annotateProgram (MkProg defs) = MkProg (reverse defs')
  where
    defs' :: [Def Ty]
    defs' = annotateDefs defs [] [TyVar (T.pack ("b" <> show i)) | i <- [(0 :: Integer) ..]]

    annotateDefs :: [Def ()] -> [Def Ty] -> [Ty] -> [Def Ty]
    annotateDefs [] acc _ = acc
    annotateDefs (Def nm args cvs body () : rest) acc fvs = do
        let (args', fvs') = annotateArgs args fvs
        let (cvs', fvs'') = annotateCvs cvs fvs'
        annotateDefs rest (Def nm args' cvs' body (head fvs'') : acc) (tail fvs'')

    annotateArgs :: [(Var, ())] -> [Ty] -> ([(Var, Ty)], [Ty])
    annotateArgs args fvs = (zip (fst <$> args) fvs, drop (length args) fvs)

    annotateCvs :: [(Var, ())] -> [Ty] -> ([(Covar, Ty)], [Ty])
    annotateCvs cvs fvs = (zip (fst <$> cvs) fvs, drop (length cvs) fvs)

-- | Generate constraints for a program.
generateConstraints :: Program () -> Either String (Program Ty, [Constraint])
generateConstraints prog = do
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

{- | Solve a set of constraints. If the constraints can be solved then
the result is a substitution of types for unification variables.
-}
solveConstraints :: [Constraint] -> Either Error (M.Map TyVar Ty)
solveConstraints cs = do
    let initial = MkSolverState cs M.empty
    case runExcept (runRWST run () initial) of
        Left err -> Left err
        Right ((), s, ()) -> Right (subst s)

{- |  The state consists of unsolved constraints and a substitution which maps
 unification variables to types.
-}
data SolverState = MkSolverState {todo :: [Constraint], subst :: M.Map TyVar Ty}

-- | The monad used for constraint solving.
type SolverM a = RWST () () SolverState (Except Error) a

{- | Add a new binding to the type substitution.
The substitution is also applied on the remaining constraints.
-}
performSubst :: TyVar -> Ty -> SolverM ()
performSubst a ty = do
    let m = M.fromList [(a, ty)]
    MkSolverState todo subst <- get
    -- perform the substitution on remaining constraints.
    let todo' = zonk m <$> todo
    -- perform the substitution on the partial solution
    let subst' = fmap (zonk m) subst
    let subst'' = M.insert a ty subst'
    put (MkSolverState todo' subst'')

-- | Add new constraints to the state of the solver.
addConstraints :: [Constraint] -> SolverM ()
addConstraints cs = modify (\(MkSolverState todos m) -> MkSolverState (cs ++ todos) m)

-- | Solve constraints until none remain.
run :: SolverM ()
run = do
    todo <- gets todo
    case todo of
        [] -> pure ()
        (c : _) -> do
            modify (\(MkSolverState todos m) -> MkSolverState (tail todos) m)
            solveConstraint c
            run

-- | Solve a single equality constraint.
solveConstraint :: Constraint -> SolverM ()
-- A constraint between a type variable and itself can be discarded.
solveConstraint (TyVar a, TyVar b) | a == b = pure ()
-- For constraints between a unification variable and a type we
-- need to perform an occurs check.
solveConstraint (TyVar a, ty)
    | a `S.member` freeTyVars ty = throwError ("Occurs check! " <> show a <> " occurs in " <> show ty)
    | otherwise = performSubst a ty
solveConstraint (ty, TyVar a)
    | a `S.member` freeTyVars ty = throwError ("Occurs check! " <> show a <> " occurs in " <> show ty)
    | otherwise = performSubst a ty
-- Int is equal to itself.
solveConstraint (IntTy, IntTy) = pure ()
-- For constraints between two lists, streams, functions, strict pairs and lazy pairs we
-- generate new subconstraints.
solveConstraint (ListTy t1, ListTy t2) = addConstraints [(t1, t2)]
solveConstraint (PairTy t1 t2, PairTy t1' t2') = addConstraints [(t1, t1'), (t2, t2')]
solveConstraint (StreamTy t1, StreamTy t2) = addConstraints [(t1, t2)]
solveConstraint (LPairTy t1 t2, LPairTy t1' t2') = addConstraints [(t1, t1'), (t2, t2')]
solveConstraint (FunTy t1 t2, FunTy t1' t2') = addConstraints [(t1, t1'), (t2, t2')]
-- In any other case we cannot unify the two types and emit an error.
solveConstraint (ty1, ty2) = throwError ("Cannot unify types: " <> show ty1 <> " and " <> show ty2)

-------------------------------------------------------------------------------
-- Type Inference
-------------------------------------------------------------------------------

-- | Infer the types of a program.
inferTypes :: Program () -> Either Error (Program Ty)
inferTypes prog = do
    -- Generate the constraints and annotate the top-level definitions
    -- with unification variables.
    (prog', constraints) <- generateConstraints prog
    -- Solve the constraints and generate a unifier which maps type variables
    -- to types.
    subst <- solveConstraints constraints
    -- Apply the unifier to the program annotated with type variables.
    pure (zonk subst prog')
