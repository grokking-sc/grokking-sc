{- |
Module      : Fun.Syntax
Description : Syntax of surface language Fun

This module defines the abstract syntax of the surface language Fun.
-}
module Fun.Syntax where

import Data.Text (Text)

-- | Variables
type Var = Text

-- | Covariables
type Covar = Text

-- | Names of top-level functions
type Name = Text

-- | Constructors
data Ctor = Nil | Cons | Tup deriving (Show, Eq)

-- | Destructors
data Dtor = Hd | Tl | Fst | Snd | Ap deriving (Show, Eq)

-- |  Binary arithmetic operators
data BinOp = Prod | Sum | Sub deriving (Show, Eq)

{- | A single clause @c(x,...) => t@ in a case or cocase term.

In a case we instantiate @a@ with 'Ctor', and in a cocase we instantiate it with 'Dtor'
-}
data Clause a = MkClause a [Var] Term deriving (Show, Eq)

-- | Terms are introduced in definitions 2.1 to 2.7 in the paper.
data Term
    = -- Introduced in definition 2.1:

      -- | Variables
      VarT Var
    | -- | Integer literals
      Lit Int
    | -- | Binary arithmetic operations
      Op Term BinOp Term
    | -- | If-zero which tests whether the first argument is equal to zero
      IfZ Term Term Term
    | -- Introduced in definition 2.3:

      -- | Let-in
      Let Var Term Term
    | -- Introduced in definition 2.4:

      -- | Toplevel function call applied to arguments
      -- The list of covariables consists of labels abstracted by the label construct
      Fun Name [Term] [Covar]
    | -- Introduced in definition 2.5:

      -- | Constructor applied to arguments
      Constructor Ctor [Term]
    | -- | Destructor applied to a scrutinee
      -- the first term is the scrutinee
      -- the dtor defines the destructor and the list of terms are the destructor arguments
      -- for all dtors except ap this list is always empty
      Destructor Term Dtor [Term]
    | -- | Case expression containing the scrutinee term and a list of patterns
      Case Term [Clause Ctor]
    | -- | Cocase expression containing only a list of copatterns
      Cocase [Clause Dtor]
    | -- Introduced in definition 2.6:

      -- | Lambda abstraction
      Lam Var Term
    | -- | Function application
      App Term Term
    | -- Introduced in definition 2.7:

      -- | Goto term with a covariable indicating where to jump to
      Goto Term Covar
    | -- | Label term introducing a label (covariable) and a term in which the label is scoped
      Label Covar Term
    deriving (Show, Eq)

{- | Toplevel functions are introduced in definition 2.4
The parameter @ty@ is used for type annotations.
-}
data Def ty = Def
    { name :: Name
    -- ^ The name of the function
    , args :: [(Var, ty)]
    -- ^ The arguments of the function; their types are inferred and annotated during type inference.
    , cont :: [(Covar, ty)]
    -- ^ The covariables which allow to pass labels to a top-level definition.
    , body :: Term
    -- ^ The function body
    , retTy :: ty
    -- ^ The return type is annotated during type inference
    }
    deriving (Show, Eq)

-- | Programs are represented as a list of top-level definitions
newtype Program ty = MkProg [Def ty] deriving (Show,Eq)

