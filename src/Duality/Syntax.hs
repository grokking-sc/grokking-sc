module Duality.Syntax where

import Data.Text


-- Variables, covariables and function names
type Var = Text

type Covar = Text

type Name = Text

-- Constructors and destructors
data Ctor = Nil | Cons | Tup deriving (Show, Eq)

data Dtor = Hd | Tl | Fst | Snd | Ap deriving (Show, Eq)

-- Binary arithmetic operators
data BinOp = Prod | Sum | Sub deriving (Show, Eq)

-- Clause in a pattern or copattern match
-- the type parameter a is used to abstract over constructors and destructors 
-- patterns are Clause Ctor and copatterns Clause Dtor
-- a clause contains 
-- the xtor (ctor or dtor)
-- any number of bound variables 
-- a bound term (the right-hand side)
--  xtor(args) => t
data Clause a = MkClause a [Var] Term deriving (Show, Eq)

-- Terms
data Term
    -- a term is one of 
    -- definition 2.1
    -- a variable 
    = VarT Var
    -- an integer literal
    | Lit Int
    -- a binary operation between two terms
    | Op Term BinOp Term
    -- a zero test 
    -- the first term is tested to be 0 
    -- the other terms are the two branches
    | IfZ Term Term Term

    -- definition 2.3
    -- a let-in term 
    -- the variable is the bound variable 
    -- the first term is the value of the variable 
    -- the second term is the scope of the variable
    | Let Var Term Term

    -- definition 2.4
    -- a toplevel function call 
    -- the name determines which definition is called 
    -- the list of terms are the arguments 
    -- and the optional covariable is the label to be propagated (if the definition uses jump/label)
    | Fun Name [Term] (Maybe Covar)

    -- definition 2.5
    -- a constructor term with constructor name and a list of arguments 
    | ConT Ctor [Term]
    -- a destructor term 
    -- the first term is the scrutinee 
    -- the dtor defines the destructor and the list of terms are the destructor arguments 
    -- for all dtors except ap this list is always empty 
    | DesT Term Dtor [Term]
    -- a case expression containing the scrutinee term and a list of patterns 
    | Case Term [Clause Ctor]
    -- a cocase expression containing only a list of copatterns
    | Cocase [Clause Dtor]
    -- a lambda abstraction with bound variable and body 

    -- definition 2.6
    | Lam Var Term
    -- a function application 
    -- the fist term is the function and the second the argument
    | App Term Term

    -- definition 2.7
    -- a jump term with a covariable indicating where to jump to 
    | Jump Term Covar
    -- a label term introducing a label (covariable) and a term in which the label is scoped
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
