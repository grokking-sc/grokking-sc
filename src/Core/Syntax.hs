{- |
Module      : Core.Syntax
Description : Syntax of intermediate language Core

This module contains the definition of the abstract syntax of the
intermediate language Core.
-}
module Core.Syntax (
    -- * Expressions
    Producer (..),
    Var,
    Consumer (..),
    Covar,
    Statement (..),
    Pattern (..),

    -- * Programs
    Def (..),
    Name,
    Program (..),
) where

import Data.Text (Text)
import Fun.Syntax (BinOp, Ctor, Dtor)

-- | Variables
type Var = Text

-- | Covariables
type Covar = Text

-- | Names of toplevel functions
type Name = Text

{- | Pattern
Definition 2.5
constructor and destructor names are parametrized by the type variable a
-}
data Pattern a = MkPattern
    { xtor :: a
    -- ^ the constcutor or destructor of the pattern
    , patv :: [Var]
    -- ^ the bound variables of the pattern
    , patcv :: [Covar]
    -- ^ the bound covariables of the pattern
    , patst :: Statement
    -- ^ the bound statnement (right hand side) of the pattern
    }
    deriving (Show, Eq)

{- | Producers
Definitions 2.1 and 2.5
-}
data Producer
    = -- | Variables
      -- Definition 2.1
      Var Var
    | -- | Integer literals
      -- Definition 2.1
      Lit Int
    | -- | Mu abstractions binding a covariable and a statement
      -- Definition 2.1
      Mu Covar Statement
    | -- | Constructor terms with producer and consumer arguments
      -- Definition 2.5
      Constructor Ctor [Producer] [Consumer]
    | -- | Cocases containing a number of destructor patterns
      -- Definition 2.5
      Cocase [Pattern Dtor]
    deriving (Show, Eq)

{- | Consumers
Definitions 2.1,2.3 and 2.5
-}
data Consumer
    = -- | Covairables
      -- Definition 2.1
      Covar Covar
    | -- | MuTilde abstractions binding a variable and statement
      -- Definition 2.3
      MuTilde Var Statement
    | -- | Case expressions containing a number of constructor patterns
      -- Definition 2.5
      Case [Pattern Ctor]
    | -- | Destructors with producer and consumer arguments
      -- Definition 2.5
      Destructor Dtor [Producer] [Consumer]
    deriving (Show, Eq)

-- | Statements
data Statement
    = -- | Cuts containing a producer and consumer (of the same type)
      -- Definition 2.1
      Cut Producer Consumer
    | -- | Binary opration with two producers, the operation and one consumer
      -- Definition 2.1
      -- The two producers are the integers on which the operation is performed
      -- the consumer is the continuation
      Op Producer BinOp Producer Consumer
    | -- | If Zero statement containing a producer and two statements
      -- Definition 2.1
      -- the producer is the integer to be tested for 0
      -- the first staement is the 0-branch, the second the non-zero branch
      IfZ Producer Statement Statement
    | -- | Toplevel function calls contining the name, producer and consumer arguments
      -- Definition 2.4
      -- The number and types of the argument are defined in the definition (added by type inference)
      Fun Name [Producer] [Consumer]
    | -- | The Done Statement
      -- Indicates finished computation
      Done
    deriving (Show, Eq)

{- | Toplevel Definitions
Definition 2.4
The type parameter is either () or Core.Ty
Before type checking it is always () afterwards always Ty
This is then used to annotate  argument types
-}
data Def a = Def
    { name :: Name
    -- ^ The name of the function
    , pargs :: [(Var, a)]
    -- ^ The producer arguments, possibly with a type
    , cargs :: [(Covar, a)]
    -- ^ The consumer arguments, possibly with a type
    , body :: Statement
    -- ^ The body of the definition
    }

-- | Programs are represented as a list of toplevel definitions
newtype Program a = MkProg [Def a]
