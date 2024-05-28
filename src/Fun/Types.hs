{-|
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
import Data.Set qualified as S
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Fun.Syntax

-- | Type variables
type TyVar = Text

-- Types
-- section 4.1, Appendix A
data Ty
    -- | Type variables
    = TyVar TyVar
    -- | The type of primitive integers
    | IntTy
    -- | The list type
    | ListTy Ty
    -- | The stream type
    | StreamTy Ty
    -- | The strict pair type
    | PairTy Ty Ty
    -- | The lazy pair type
    | LPairTy Ty Ty
    -- | The function type
    | FunTy Ty Ty
    deriving (Show, Eq)

-- | Compute the set of free type variables of a type.
-- There are no ways to bind a type variable so any type variable that appears is always free
freeTyVars :: Ty -> S.Set TyVar
freeTyVars (TyVar v) = S.singleton v
freeTyVars IntTy = S.empty
freeTyVars (ListTy t) = freeTyVars t
freeTyVars (PairTy t1 t2) = S.union (freeTyVars t1) (freeTyVars t2)
freeTyVars (StreamTy t) = freeTyVars t
freeTyVars (LPairTy t1 t2) = S.union (freeTyVars t1) (freeTyVars t2)
freeTyVars (FunTy t1 t2) = S.union (freeTyVars t1) (freeTyVars t2)

-- | A typeclass for applying type substitutions.
-- replaces all type variables in the map with the type in the map for a given argument of type a
class Zonk a where
    zonk :: M.Map TyVar Ty -> a -> a

-- replacing type variables in a type
instance Zonk Ty where
    -- a type variable is replaced if it is found in the map, otherwise it is left as-is
    zonk m (TyVar a) = case M.lookup a m of
        Nothing -> TyVar a
        Just ty -> ty
    -- integer types can be directly returned as they contain no variables
    zonk _ IntTy = IntTy
    -- for all other types, substitution needs to be performed on all arguments
    zonk m (ListTy t) = ListTy (zonk m t)
    zonk m (PairTy t1 t2) = PairTy (zonk m t1) (zonk m t2)
    zonk m (StreamTy t) = StreamTy (zonk m t)
    zonk m (LPairTy t1 t2) = LPairTy (zonk m t1) (zonk m t2)
    zonk m (FunTy t1 t2) = FunTy (zonk m t1) (zonk m t2)

-- replacing type variables in definitions
-- this can only be done if the definition actually contains a type
-- thus the instance is only defined for (Def Ty) and not for any (Def a)
instance Zonk (Def Ty) where
    -- types are included in arguments and the return type
    -- so replacing type variables amounts to replacing them in the argument types and the return type
    zonk m (Def name args cv body ret) =
        Def name ((\(x,t) -> (x, zonk m t)) <$> args) cv body (zonk m ret)

-- replacing type variables in a program
instance Zonk (Program Ty) where
    -- this amounts to replacing all type variables in definitions
    zonk m (MkProg defs) = MkProg (zonk m <$> defs)

-- replacing type variables in typing constraints
-- these are generated during type inference (see below)
instance Zonk Constraint where
    -- replace type variables in both included types
    zonk m (t1,t2) = (zonk m t1, zonk m t2)

-- a type equality constraint between two types
type Constraint = (Ty, Ty)

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

-- errors during constraint generation
type Error = String

-- the generator monad
-- uses the RWS and Except monad
-- RWS contains the environment GenReader,
-- an output to write to (), here nothing
-- and a state GenState
-- Using the transformer RWST it is combined with the Except monad to add errors
type GenM a = RWST GenReader () GenState (Except Error) a

-- add a list of variables or covariables to a GenReader
-- used to locally update the environment
addVarBindings :: [(Var, Ty)] -> GenReader -> GenReader
addVarBindings xs (env, coenv, prog) = (M.union (M.fromList xs) env, coenv, prog)

addCovarBinding :: Covar -> Ty -> GenReader -> GenReader
addCovarBinding v ty (env, coenv, prog) = (env, M.insert v ty coenv, prog)

-- generate a fresh type variable
-- uses the integer in the monad state to ensure every new variable is unique
-- type vars always have the format ai with i being the current number
freshVar :: GenM Ty
freshVar = do
    (i, _) <- get
    modify (\(j, cs) -> (j + 1, cs))
    pure (TyVar (T.pack("a" <> show i)))

-- adds a constraint to the monad state
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


-- Appendix A
-- generate constraints for a term
-- constraints are always added to the current state
-- the type (possibly a type variable or containing a type variable) of the term is returned
genConstraintsTm :: Term -> GenM Ty
-- Variables
genConstraintsTm (VarT v) = do
    -- a variable needs to be contained in the environment
    -- namely the variable map
    -- if it is not, typing fails, otherwise the type is the one that was looked up
    -- no constraints need to be generated
    --
    --  v:τ ∈ Γ
    -- ―――――――――
    -- Γ ⊢v : τ
    --
    --
    (env, _, _) <- ask
    case M.lookup v env of
        Nothing -> throwError ("Variable " <> show v <> " not bound in environment.")
        Just ty -> pure ty
-- Arithmetic Expressions
-- integer literals always have integer type
-- no constraints are generated
--
-- ―――――――――――
-- Γ ⊢ n : Int
--
genConstraintsTm (Lit _) = pure IntTy
--  Γ⊢t1:Int  Γ⊢t2:Int
-- ――――――――――――――――――――
--   Γ ⊢ t1*t2 : Int
--
genConstraintsTm (Op t1 _ t2) = do
    -- first generate constraints for the first term
    ty1 <- genConstraintsTm t1
    -- the first term needs to be int
    addConstraint (ty1, IntTy)
    -- generate constraints for the second term
    -- and ensure it is also int
    ty2 <- genConstraintsTm t2
    addConstraint (ty2, IntTy)
    -- binary operations always have type int
    pure IntTy
--
--  Γ⊢t1:Int  Γ⊢t2:τ  Γ⊢t3:τ
-- ――――――――――――――――――――――――――
--   Γ ⊢ IfZ(t1,t2,t3) : τ
--
genConstraintsTm (IfZ t1 t2 t3) = do
    -- the type of the first term needs to be int
    ty1 <- genConstraintsTm t1
    addConstraint (ty1, IntTy)
    -- the other two terms can have an arbitary type
    -- however, their type needs to be equal
    ty2 <- genConstraintsTm t2
    ty3 <- genConstraintsTm t3
    addConstraint (ty2, ty3)
    pure ty2
-- Let Expressions
--
--      Γ⊢t1:τ  Γ,x:τ⊢t2:σ
-- ――――――――――――――――――――――――――
--   Γ ⊢ Let x=t1 in t2 : τ
--
genConstraintsTm (Let v t1 t2) = do
    -- the type of the first term (the value of the variable) needs to available while generating constraints for the second
    -- since v is only in scope while checking t2 we use local to udpate the envionment only during genConstraintsTm t2
    -- the type of the let binding is the type of the second term, so we can directly return the result of constraints generation for t2
    ty1 <- genConstraintsTm t1
    local (addVarBindings [(v, ty1)]) $ genConstraintsTm t2
-- Toplevel Function Calls
-- Function calls without covariable argument
--
--   f(x_i:τ_i;a:τ'):σ ∈ P  Γ⊢t_i:τ_i  Γ⊢a:τ'
-- ―――――――――――――――――――――――――――――――――――――――――
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
        then throwError ("Function " <> T.unpack nm <> " called with wrong number of arguments. Expected: " <> show (length argtys) <> " Got: " <> show (length args))
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
        then throwError ("Function " <> T.unpack nm <> " called with wrong number of arguments. Expected: " <> show (length argtys) <> " Got: " <> show (length args))
        else do
            -- now we also need to find the type of the covariable
            (_,coenv,_) <- ask
            -- if it is in the environment, use that type
            -- otherwise create a new type variable
            cvTy <- case M.lookup cv coenv of Nothing -> freshVar; Just ty -> pure ty
            -- generate constraints for all arguments as before
            -- but now locally add the type of the covariable to the environment during constraint generation
            -- this ensures that whenever cv is used in an argument, it's type will be the same
            tys <- forM args (local (addCovarBinding cv cvTy) . genConstraintsTm )
            -- again add a constraint for each argument
            forM_ (zip argtys tys) addConstraint
            -- and return the defined return type
            pure retty

-- Data Types
-- Constructors
-- List
-- An empty list can have any type argument
-- so we need to generate a new type variable
--
--
-- ――――――――――――――――――――――――――
--   Γ ⊢ Nil : List(τ)
--
genConstraintsTm (ConT Nil []) = ListTy <$> freshVar
--
--  Γ ⊢ t1: τ  Γ ⊢ t2: List(τ)
-- ―――――――――――――――――――――――――――――
--   Γ ⊢ Cons(t1,t2) : List(τ)
--
genConstraintsTm (ConT Cons [t1, t2]) = do
    -- given Cons(t1 t2),
    -- t1 can have any type
    -- t2 needs to have a list type
    -- the type argument of the list needs to be the same as type of t1
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    addConstraint (ListTy ty1, ty2)
    pure ty2
-- Pair
--
--  Γ ⊢ t1: τ     Γ ⊢ t2: σ
-- ―――――――――――――――――――――――――――――
--   Γ ⊢ Tup(t1,t2) : Pair(τ,σ)
--
genConstraintsTm (ConT Tup [t1, t2]) = do
    -- the contents of a tuple can have any type
    -- these types are the arguments of the pair type
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    pure (PairTy ty1 ty2)
-- given any other constructor term, typing always fails
-- this means the above three patterns ensure correct arities for the constructors
genConstraintsTm (ConT ctor _) =
    throwError ("Constructor " <> show ctor <> " applied to wrong number of arguments.")
-- Cases
-- Lists
-- the pattern here ensures
-- a nil pattern never has an argument
-- a cons pattern always has 2 arguments
--
--  Γ⊢t:List(σ)     Γ⊢t_nil:τ     Γ,x:σ,xs:List(σ) ⊢ t_cons:τ
-- ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
--   Γ ⊢ case t of { Nil => t_nil, Cons(x,xs) => t_cons } : τ
--
genConstraintsTm (Case t [MkClause Nil [] t_nil, MkClause Cons [x, xs] t_cons]) = do
    -- first generate constraints for the scrutinee term
    ty <- genConstraintsTm t
    -- this type needs to be a list type for the pattern to be well formed
    -- but since we do not know what kind of list we generate a type variable for the argument
    a <- freshVar
    addConstraint (ty, ListTy a)
    -- generate constraints for the right-hand sides
    -- for nil they can be generated directly
    retNil <- genConstraintsTm t_nil
    -- for cons we have two bound variables
    -- these need to be added to the envrionment during constraint generation for the right-hand side
    -- since the types of Cons(t1,t2) need to be t1:ty, ty2:list ty we add x:a and xs:list a to the environment
    -- the type variable a generated above is reused here, since the type of the srcutinee needs to match the types of bound variables
    -- during this constraint generations constraints on a are generated which then ensures a can be substituted
    retCons <- local (addVarBindings [(x, a), (xs, ListTy a)]) (genConstraintsTm t_cons)
    -- the two right-hand side types need to be equal
    addConstraint (retNil, retCons)
    -- the type of the entire term is the type of the right-hand sides
    pure retNil
-- Pairs
-- this pattern ensures cases on pairs always have only a pattern for Tup
-- and that pattern always has two arguments
--
--  Γ⊢t:Pair(σ,σ')     Γ,x:σ,y:σ'⊢body:τ
-- ――――――――――――――――――――――――――――――――――――――――――
--   Γ ⊢ case t of { Tup(x,y) => body } : τ
--
genConstraintsTm (Case t [MkClause Tup [x, y] body]) = do
    -- generate constaints for the scrutinee
    -- and ensure it has a pair type
    -- the arguments are unclear for now so we use type variables
    ty1 <- genConstraintsTm t
    a <- freshVar
    b <- freshVar
    addConstraint (ty1, PairTy a b)
    -- generate constraints for the left hand side
    -- to ensure the type variables can be resolved and both x and y are only used with these types we again add them to the environment
    -- the final type then is the type of the right-hand side
    local (addVarBindings [(x, a), (y, b)]) (genConstraintsTm body)
-- no other cases are allowed
-- thus the above two patterns ensure cases always have the correct number of patterns, all with correct number of arguments
genConstraintsTm tm@(Case _ _) = throwError ("Invalid case expression: " <> show tm)
-- Codata Types
-- Destructors
-- Stream
--
--  Γ⊢t:Stream(τ)
-- ―――――――――――――――
--   Γ ⊢ t.hd : τ
--
genConstraintsTm (DesT t Hd []) = do
    -- the type of the scrutinee needs to be a stream type
    -- this is analogous to cases above
    -- since we do not know the contents of the stream we generate a
    ty <- genConstraintsTm t
    a <- freshVar
    addConstraint (ty, StreamTy a)
    -- the head of that stream has the type of the type argument
    pure a

--
--      Γ⊢t:Stream(τ)
-- ――――――――――――――――――――――――
--   Γ ⊢ t.tl : Stream(τ)
--
genConstraintsTm (DesT t Tl []) = do
    -- analogous to head
    -- we again require the scrutinee to have type stream a
    -- and need a type variable for the argument
    ty <- genConstraintsTm t
    a <- freshVar
    addConstraint (ty, StreamTy a)
    -- now the return type is stream a
    -- since the tail of a stream is another stream
    pure (StreamTy a)
-- lazy pairs
--
--      Γ⊢t:LPair(τ,σ)
-- ――――――――――――――――――――――――
--      Γ ⊢ t.fst : τ
--
genConstraintsTm (DesT t Fst []) = do
    -- the scrutinee needs to have type lpair a b
    -- a and b are unclear for now, so we generate type variables for them
    ty <- genConstraintsTm t
    a <- freshVar
    b <- freshVar
    addConstraint (ty, LPairTy a b)
    -- the type of the first element of a lazy pair is then the first type argument
    pure a
--
--     Γ⊢t:lpair(τ,σ)
-- ――――――――――――――――――――――――
--    Γ ⊢ t.snd : σ
--
genConstraintsTm (DesT t Snd []) = do
    -- analogous to fst
    -- t needs to have type lpair a b with a b unclear
    -- so type variables are generated for a and b
    ty <- genConstraintsTm t
    a <- freshVar
    b <- freshVar
    addConstraint (ty, LPairTy a b)
    -- the type of the second element of a lazy pair is then the second type argument
    pure b
-- all other destructor terms are rejected
-- so as with cosntructors this ensures argument arity
genConstraintsTm (DesT _ dtor _) =
    throwError ("Destructor " <> show dtor <> " called with wrong number of arguments")
-- cocases
-- streams
-- a stream cocase needs to have patterns for head and tail
-- both of these destructors take no arguments
--
--     Γ⊢t1:τ             Γ⊢t2:Stream(τ)
-- ――――――――――――――――――――――――――――――――――――――――――――――――
--    Γ ⊢ cocase { hd => t1, tl => t2 } : Stream(τ)
--
genConstraintsTm (Cocase [MkClause Hd [] t1, MkClause Tl [] t2]) = do
    -- the type of the hd copattern is arbitrary
    ty1 <- genConstraintsTm t1
    -- the type of the tl copattern needs to be a stream
    -- and the type argument of that stream needs to be equal to ty1
    ty2 <- genConstraintsTm t2
    addConstraint (StreamTy ty1, ty2)
    -- a cocase then has the stream type with argument ty1
    pure (StreamTy ty1)
-- lazy pairs
-- a lazy pair cocase needs to have two patterns, one for fst and one for snd
-- both of these destructors take no arguments
----
--     Γ⊢t1:τ             Γ⊢t2:σ
-- ――――――――――――――――――――――――――――――――――――――――――――――――
--    Γ ⊢ cocase { fst => t1, snd => t2 } : LPair(τ,σ)
--
genConstraintsTm (Cocase [MkClause Fst [] t1, MkClause Snd [] t2]) = do
    -- both the types of the right-hand sides are arbitrary
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    -- the final type is the lazy pair with these two types as arguments
    pure (LPairTy ty1 ty2)
-- all other cocases are rejected
-- as before this ensures the correct patterns with correct number of arguments
genConstraintsTm tm@(Cocase _) = throwError ("Invalid cocase expression: " <> show tm)
-- Function Type
--
--     Γ,v:τ⊢t:σ
-- ――――――――――――――――――――――
--    Γ ⊢  λv.t : τ -> σ
--
genConstraintsTm (Lam v t) = do
    -- the argument type of a function type is arbitrary, so we generate a type variable
    a <- freshVar
    -- this type variable is then added to the environment while inferring the type of the body
    ty <- local (addVarBindings [(v, a)]) $ genConstraintsTm t
    -- the finaly type is the type a->ty
    pure $ FunTy a ty
--
--     Γ⊢t1:τ->σ    Γ⊢t2:τ
-- ――――――――――――――――――――――――――
--       Γ ⊢  t1 t2 : σ
--
genConstraintsTm (App t1 t2) = do
    -- the type of t1 needs to be a function type
    -- the return type of that function type is unclear so we generate the type variable ret
    -- and we add the constraint ty1=ty2->ret
    ty1 <- genConstraintsTm t1
    ty2 <- genConstraintsTm t2
    ret <- freshVar
    addConstraint (ty1, FunTy ty2 ret)
    -- the final type is the return type of the function type
    pure ret
-- Jump / Label
--
--
--     Γ⊢t:τ    v:σ∈Γ
-- ――――――――――――――――――――――――――
--     Γ ⊢  Jump(t,v) : σ
--
genConstraintsTm (Goto t v) = do
    -- get the type of t
    ty1 <- genConstraintsTm t
    -- get the type of the label
    ty2 <- do
        -- when the label is in the environment, this is the type in the environment
        -- otherwise typing fails, as the label is not in scope
        (_, coenv, _) <- ask
        case M.lookup v coenv of
            Nothing -> throwError ("Covariable " <> show v <> " not bound in environment")
            Just ty -> pure ty
    -- the types of t and the label need to be equal
    addConstraint (ty1, ty2)
    -- the type of the goto itself is arbitrary
    -- note that if typing v succeeds it is ensured that the goto term is contained in some term
    -- and that this term contains a label definition
    -- by the below case for labels this means this type can be resolved
    freshVar
--
--         Γ,v:τ⊢t:τ
-- ――――――――――――――――――――――――
--     Γ ⊢  Label(v,t):τ
--
genConstraintsTm (Label v t) = do
    -- the label can have any type
    -- so we generate a variable while generating the constraints for the scoped term
    a <- freshVar
    ty <- local (addCovarBinding v a) (genConstraintsTm t)
    -- the type of the variable also needs to be equal to the type of the scoped term
    addConstraint (a, ty)
    -- and this is the type of the entire term
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
    let initialState = (0,[])
    let act :: GenM ()
        act = forM_ defs genConstraintsDef
    case runExcept (runRWST act initialReader initialState) of
        Left err -> Left err
        Right (_,(_,cs),_) -> pure (prog',cs)


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
        Right ((),s,()) -> Right (subst s)

-- state for the solver monad
-- contains constraints that have not been resolved yet
-- and a map of typevariables to types, which contains the already resolved variables
data SolverState = MkSolverState { todo :: [Constraint], subst :: M.Map TyVar Ty }

-- the solver monad
-- again uses the RWST monad transformer with the Except monad
-- now the environment and writer are both empty and the state is the solverstate
type SolverM a = RWST () () SolverState (Except Error) a


-- adds a new type variable to the state (the typevar map)
-- before adding the new substitution, the substitution is performed on the remaining constraints
performSubst :: TyVar -> Ty -> SolverM ()
performSubst a ty = do
    -- generate a new variable map containing only the given variable and type
    let m = M.fromList [(a,ty)]
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

-- add another constraint to the state
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
        (c:_) -> do
            modify (\(MkSolverState todos m) -> MkSolverState (tail todos) m)
            solveConstraint c
            run

-- solve a single constraints
solveConstraint :: Constraint -> SolverM ()
-- a constraint between a type variable and itself can be discarded
solveConstraint (TyVar a, TyVar b) | a == b = pure ()
-- a constraint between a type var and another type
-- check if the typevar appears free in the other type
-- if it does this is a recursive type, which is not supported
-- thus an error is thrown
-- otherwise, the variable can be substituted by the given type in all constraints and added to the variable map
solveConstraint (TyVar a,ty) | a `S.member` freeTyVars ty = throwError ("Occurs check! " <> show a <> " occurs in " <> show ty)
                             | otherwise = performSubst a ty
solveConstraint (ty,TyVar a) | a `S.member` freeTyVars ty = throwError ("Occurs check! " <> show a <> " occurs in " <> show ty)
                             | otherwise = performSubst a ty
-- two int types are always equal
solveConstraint (IntTy, IntTy) = pure ()
-- two lists are equal if their arguments are equal
-- so we can just add a constraint between the arguments
solveConstraint (ListTy t1,ListTy t2) = addConstraints [(t1,t2)]
-- two pairs are equal when both their arguments are equal
solveConstraint (PairTy t1 t2, PairTy t1' t2') = addConstraints [(t1,t1'),(t2,t2')]
-- two streams are equal when both their arguments are equal
solveConstraint (StreamTy t1,StreamTy t2) = addConstraints [(t1,t2)]
-- two lazy pairs are equal when their type arguments are equal
solveConstraint (LPairTy t1 t2, LPairTy t1' t2') = addConstraints [(t1,t1'),(t2,t2')]
-- two functions are equal when both their argument and return types are equal
solveConstraint (FunTy t1 t2, FunTy t1' t2') = addConstraints [(t1,t1'),(t2,t2')]
-- in any other case the constraint is between two unqeual types so the solver fails
solveConstraint (ty1,ty2) = throwError ("Cannot unify types: " <> show ty1 <> "and" <> show ty2)


-------------------------------------------------------------------------------
-- Type Inference
-------------------------------------------------------------------------------

-- combine all the above monad to infer types of a program
inferTypes :: Program () -> IO (Either Error (Program Ty))
inferTypes prog = do
    -- first generate constraints
    -- this first adds type variables to definitions then generates type constraints
    case generateConstraints prog of
        -- when constraint generation fails typing failed
        -- otherwise we get a program with added types (all type variables) and a list of constraints
        Left err -> pure (Left err)
        Right (prog', constraints) -> do
            print constraints
            -- next we solve the constraints
            case solveConstraints constraints of
                -- when constraint solving fails, typing fails
                -- otherwise we get a substitution for type variables
                -- we then substitute type variables in the program using this map
                -- and are done
                Left err -> pure (Left err)
                Right subst -> do
                    print subst
                    pure (Right (zonk subst prog'))
