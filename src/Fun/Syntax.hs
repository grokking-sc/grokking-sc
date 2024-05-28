{-|
Module      : Fun.Syntax
Description : Syntax of surface language Fun

This module contains the definition of the abstract syntax of the
surface language Fun.
-}
module Fun.Syntax where

import Data.Text (Text)

-- | Variables
type Var = Text

-- | Covariables
type Covar = Text

-- | Names of toplevel functions
type Name = Text

-- | Constructors
data Ctor = Nil | Cons | Tup deriving (Show, Eq)

-- | Destructors
data Dtor = Hd | Tl | Fst | Snd | Ap deriving (Show, Eq)

-- | Binary arithmetic operators
data BinOp = Prod | Sum | Sub deriving (Show, Eq)

-- | Clause in a pattern or copattern match
-- the type parameter a is used to abstract over constructors and destructors
-- patterns are Clause Ctor and copatterns Clause Dtor
-- a clause contains
-- the xtor (ctor or dtor)
-- any number of bound variables
-- a bound term (the right-hand side)
--  xtor(args) => t
data Clause a = MkClause a [Var] Term deriving (Show, Eq)

-- | Terms
--
-- Terms are introduced in definitions 2.1 to 2.7 in the paper.
data Term
    -- Introduced in definition 2.1:

    -- | Variables
    = VarT Var
    -- | Integer literals
    | Lit Int
    -- | Binary arithmetic operations
    | Op Term BinOp Term
    -- | If-zero which tests whether the first argument is equal to zero
    | IfZ Term Term Term

    -- Introduced in definition 2.3:

    -- | Let-in
    | Let Var Term Term

    -- Introduced in definition 2.4:

    -- | Toplevel function call applied to arguments
    -- The optional covariable is the label to be propagated (if the definition uses goto/label)
    | Fun Name [Term] (Maybe Covar)

    -- Introduced in definition 2.5:

    -- | Constructor applied to arguments
    | ConT Ctor [Term]
    -- | Destructor applied to a scrutinee
    -- the first term is the scrutinee
    -- the dtor defines the destructor and the list of terms are the destructor arguments
    -- for all dtors except ap this list is always empty
    | DesT Term Dtor [Term]
    -- | Case expression containing the scrutinee term and a list of patterns
    | Case Term [Clause Ctor]
    -- | Cocase expression containing only a list of copatterns
    | Cocase [Clause Dtor]

    -- Introduced in definition 2.6:

    -- | Lambda abstraction
    | Lam Var Term
    -- | Function application
    | App Term Term

    -- Introduced in definition 2.7:

    -- | Goto term with a covariable indicating where to jump to
    | Goto Term Covar
    -- | Label term introducing a label (covariable) and a term in which the label is scoped
    | Label Covar Term
    deriving (Show, Eq)


-- definition 2.4
-- Toplevel definitions
-- the type variable a is used for types
-- after parsing a is always ()
-- after typechecking it is ty
data Def a = Def {
  -- a toplevel definition containts
  -- its name
  name :: Name,
  -- a list of arguments
  -- with an optional type
  args :: [(Var, a)],
  -- an optional covariable
  -- this is the label that is propagated within the body
  cont :: Maybe Covar,
  -- a body
  body :: Term,
  -- a return type
  -- added during typing
  retTy :: a} deriving (Show, Eq)

-- Programs
-- contains a list of definitions
newtype Prog a = MkProg [Def a] deriving (Show, Eq)
