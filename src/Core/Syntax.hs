module Core.Syntax where
import Data.Text (Text)
import Fun.Syntax (BinOp, Ctor, Dtor)


-- type definitions for variables, covariables and names
type Var = Text

type Covar = Text

type Name = Text

-- definition 2.5
-- constructor and destructor names are parametrized by the type variable a
-- xtor(patv;patcv) => patst
data Pattern a = MkPattern
    -- a pattern contains:
    -- an xtor (either constructor or destructor name)
    { xtor :: a
    -- bound varibles
    , patv :: [Var]
    -- bound covariables
    , patcv :: [Covar]
    -- a bound statement
    , patst :: Statement
    }
    deriving (Show, Eq)


data Producer
    -- definition 2.1
    -- a producer is one of
    -- a variable
    = Var Var
    -- an integer literal
    | Lit Int
    -- a mu abstraction binding a covariable and a statement
    -- mu cv. st
    | Mu Covar Statement

    -- definition 2.5
    -- a constructor term with producer and consumer arguments
    -- ctor(prods,cons)
    | Constructor Ctor [Producer] [Consumer]
    -- a cocase containing a number of destructor patterns
    -- cocase { pts }
    | Cocase [Pattern Dtor]
    deriving (Show, Eq)

data Consumer
    -- definition 2.1
    -- a consumer is one of
    -- a covairable
    = Covar Covar

    -- definition 2.3
    -- a mu-tilde abstraction binding a variable and statement
    -- mu~ v.st
    | MuTilde Var Statement

    -- definition 2.5
    -- a case expression containing a number of constructor patterns
    | Case [Pattern Ctor]
    -- a destructor with producer and consumer arguments
    -- dtor(prods;cons)
    | Destructor Dtor [Producer] [Consumer]
    deriving (Show, Eq)

data Statement

    -- definition 2.1
    -- a statement is one of
    -- a cut between a prodcuer and consumer (of the same type)
    -- <prd | cns>
    = Cut Producer Consumer
    -- a binary operation with
    -- two producer arguments (of type int), that are the arguments to the operation
    -- a binary operation (+.-....) to be performed
    -- a consumer argument serving as the continuation of the statement
    -- *(p1,p2;c)
    | Op Producer BinOp Producer Consumer
    -- a zero test contains a producer to be tested and two branch statements
    -- ifz(n,st1,st2)
    | IfZ Producer Statement Statement

    -- definition 2.4
    -- a toplevel call contains the name of the definition and lists of producer and consumer arguments
    -- these need to fit with the definition
    -- fun(prods;cons)
    | Fun Name [Producer] [Consumer]
    -- the Done command indicating a finished computation
    | Done
    deriving (Show, Eq)

-- definition 2.4
-- toplevel definitions
-- the type parameter a parameterizes extre information about the arguments
-- this is used to annotate their types (done by the type checker of duality)
data Def a = Def {
  -- a definition contains
  -- a name, used to call the definition
  name :: Name,
  -- a number of producer arguments, represented by variables
  -- the additional argument a contains the type of that argument
  pargs :: [(Var, a)],
  -- a number of consumer arguments, represnted by covariables
  -- the additional argument a contains the type of that argument
  cargs :: [(Covar, a)],
  -- a body, which is a statement
  -- this statement will be substituted for the name during evaluation
  -- within the statement all variables and covariables defined in pargs and cargs can be used
  -- these will then be replaced by the arguments used to call the definition
  body :: Statement}

-- a program is a list of definitions
newtype Prog a = MkProg [Def a]
