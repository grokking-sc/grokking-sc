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
-}
data Pattern a = MkPattern
    { xtor :: a
    -- ^ Constructor or destructor
    , patv :: [Var]
    -- ^ Bound variables
    , patcv :: [Covar]
    -- ^ Bound covariables
    , patst :: Statement
    -- ^ Right hand side
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
    | -- | Mu abstractions
      -- Definition 2.1
      Mu Covar Statement
    | -- | Constructors
      -- Definition 2.5
      Constructor Ctor [Producer] [Consumer]
    | -- | Cocases
      -- Definition 2.5
      Cocase [Pattern Dtor]
    deriving (Show, Eq)

{- | Consumers
Definitions 2.1,2.3 and 2.5
-}
data Consumer
    = -- | Covariables
      -- Definition 2.1
      Covar Covar
    | -- | MuTilde abstractions
      -- Definition 2.3
      MuTilde Var Statement
    | -- | Case expressions
      -- Definition 2.5
      Case [Pattern Ctor]
    | -- | Destructors
      -- Definition 2.5
      Destructor Dtor [Producer] [Consumer]
    deriving (Show, Eq)

-- | Statements
data Statement
    = -- | Cuts
      -- Definition 2.1
      Cut Producer Consumer
    | -- | Binary operation on primitive integers
      -- Definition 2.1
      Op Producer BinOp Producer Consumer
    | -- | If Zero statement
      -- Definition 2.1
      IfZ Producer Statement Statement
    | -- | Toplevel function calls
      -- Definition 2.4
      Fun Name [Producer] [Consumer]
    | -- | Done
      -- Indicates a finished computation
      Done
    deriving (Show, Eq)

{- | Toplevel Definitions
Definition 2.4
-}
data Def a = Def
    { name :: Name
    -- ^ The name of the function
    , pargs :: [(Var, a)]
    -- ^ Producer arguments
    , cargs :: [(Covar, a)]
    -- ^ Consumer arguments
    , body :: Statement
    -- ^ The body of the definition
    }

-- | Programs are represented as a list of toplevel definitions
newtype Program a = MkProg [Def a]
