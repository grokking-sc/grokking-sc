{- |
Module      : Core.Focusing
Description : Focus expressions in the core language

This module implements focusing for expressions in the core language.
-}
module Core.Focusing (
    -- * Focussing

    -- Section 3
    Focus (..),
    isValue,
) where

import Core.Substitution
import Core.Syntax
import Data.List (find)

{- | Tests if a producer is a value
literals, variables and cocase are always values
Constructors are values if all their arguments are values
Mu-abstractions are no values
-}
isValue :: Producer -> Bool
{-
>>> isValue (Lit 1)
True
-}
isValue (Lit _) = True
{-
>>> isValue (Var "x")
True
-}
isValue (Var _) = True
{-
>>> isValue (Cocase []) = True
-}
isValue (Cocase _) = True
{-
>>> isValue (Constructor Nil [] [])
True
>>> isValue (Constructor Cons [(Mu x.Done),(Constructor Nil [] [])])
False
 -}
isValue (Constructor _ prds _) = all isValue prds
{-
>>> isValue (Mu "x" Done)
False
 -}
isValue (Mu _ _) = False

{- | Type class for focusing different expressions (producers, consumers, etc)
Definition 3.2
-}
class Focus a where
    -- | Focus an expression
    focus :: a -> a

{- | Focusing instance for patterns
Focuses the statement bound in the pattern
-}
instance Focus (Pattern a) where
    focus :: Pattern a -> Pattern a
    {-
    >>> focus (MkPattern Nil [] [] Done)
    MkPattern Nil [] [] Done
    -}
    focus (MkPattern nm v cv s) = MkPattern nm v cv (focus s)

{- | Focusing instance for producers
Except for constructor terms, this only recursively focuses subexpressions
-}
instance Focus Producer where
    focus :: Producer -> Producer
    {-
    >>> focus (Var "x")
    Var "x"
     -}
    focus (Var x) = Var x
    {-
    >>> focus (Lit 3)
    Lit 3
     -}
    focus (Lit n) = Lit n
    {-
    >>> focus (Mu "x" Done) = Mu "x" Done
     -}
    focus (Mu x s) = Mu x (focus s)
    {-
     focus (Cocase [MkPattern Fst [] ["a"] Done])
     Cocase [MkPattern Fst '[] ["a"] Done]
    -}
    focus (Cocase cocases) = Cocase (focus <$> cocases)
    -- in a constructor term, focusing amounts to replacing the first non-value argument by a variable
    -- this way, that argument is evaluated first and then the variable is substituted by the evaluated argument
    {-
     >>> focus (Constructor Cons [Lit 1,Nil] [])
     Constructor Cons [Lit 1, Nil] []

     >>> focus (Constructor Cons [mu x.Ifz(Lit 1, Done, Done),Nil] [])
     Mu "a0" (Cut (mu x.IfZ(Lit 1, Done, Done)) (Mutilde "x0" (Cut (Constructor Cons [(Var "x0"),Nil] []) (Covar "a0"))))
     -}
    focus cont@(Constructor ct pargs cargs) =
        -- find the first non-value argument
        case find (not . isValue) pargs of
            --- when all are values, all arguments are focused
            Nothing -> Constructor ct (focus <$> pargs) (focus <$> cargs)
            -- otherwise, we have to replace the argument by a variable
            Just p1' -> do
                -- generate the variable to replace p1'
                let v = freshVar [cont]
                -- generate the covariable for surrounding mu abstraciton
                let cv = freshCovar [cont]
                -- replace p1' by v in producer arguments
                -- Mu-abstractions are no values
                let newArgs = (\p -> if p == p1' then Var v else p) <$> pargs
                -- the result is a mu abstraction with cv as variable
                -- this is needed, so the result is still a producer
                -- the statement bound by mu has producer p1' (after focusing p1' again)
                -- the consumer of this statement is a mutilde abstraction
                -- this means evaluating will then replace v with p1' (after p1' has been evaluated to a value)
                -- the statment of that mutilde expression has producer that is the original constructor with v instead of p1'
                -- its consumer is the bound covariable bound by the outer mu abstraction
                -- this covariable acts as a continuatiion, allowing all to be evaluated in the correct order
                Mu cv (Cut (focus p1') (MuTilde v (Cut (focus (Constructor ct newArgs cargs)) (Covar cv))))

{- | Focusing instance for consumers
As with producers, except for destructor terms, this only recursively focuses subexpressions
-}
instance Focus Consumer where
    focus :: Consumer -> Consumer
    {-
    >>> focus (Covar "a")
    Covar "a"
    -}
    focus (Covar x) = Covar x
    {-
    >>> focus (MuTilde "x" Done)
    MuTilde "x" Done
    -}
    focus (MuTilde x s) = MuTilde x (focus s)
    {-
    >>> focus (Case [MkPattern Nil [] [] Done)]
    Case [MkPattern Nil [] [] Done]
     -}
    focus (Case cases) = Case (focus <$> cases)
    -- focusing a destructor term is analogous to focusing a constructor term
    -- we find the first non-value producer argument and replace it by a variable v
    {-
    >>> focus (Destructor Ap [Var "x"] [Covar "a"])
    Destructor Ap [Var "x"] [Covar "a"]
    >>> focus (Destructor Ap [Mu "b". (Op (Lit 1) Sum (Lit 1) (Covar "b"))] [Covar "a"]
    MuTilde "x0" (Cut (Mu "b" (Op (Lit 1) (Lit 1) (Covar "b"))) (Destructor Ap [Var "x0"] [Covar "a"]))
     -}
    focus dest@(Destructor dt pargs cargs) =
        -- find the first non-value producer argument
        case find (not . isValue) pargs of
            -- when all producer arguments are values, only focus the arguments and leave the destructor untouched
            Nothing -> Destructor dt (focus <$> pargs) (focus <$> cargs)
            -- otherwise replace p1'
            Just p1' -> do
                -- generate variable to replace p1' with
                let v = freshVar [dest]
                -- replace p1' by v in pargs
                let newArgs = (\p -> if p == p1' then Var v else p) <$> pargs
                -- the returned focused consumer is a mu-tilde abstraction with v as bound variable
                -- this means when p1' has been evaluated to a value it is reinserted for v
                -- since mutilde already defines a consumer, we do not need an additional mu abstraction
                -- the cut of this abstraction then contains p1' focused and the destructor with replaced aruments
                MuTilde v (Cut (focus p1') (Destructor dt newArgs cargs))

{- | Focusing instance for statements
Ifz and binary operations are focused analogously to destructors and constructors
-}
instance Focus Statement where
    focus :: Statement -> Statement
    {-
    >>> focus (Cut (Var "x") (Covar "a")
    Cut (Var "x") (Covar "a")
     -}
    focus (Cut p c) = Cut (focus p) (focus c)
    -- in a binary operation focusing depends on which arguments are values
    {-
    >>> focus (Op (Lit 1) Sum (Lit 2) (Covar "a")
    Op (Lit 1) Sum (Lit 2) (Covar "a")

    >>> focus (Op (Lit 1) Sum (Mu "x" (Cut (Lit 1) (Covar "x"))) (Covar "a")
    Cut (Mu "x" (Cut (Lit 1) (Covar "x"))) (MuTilde "x0" (Op (Lit 1) Sum (Var "x0")))

    >>> focus (Op  (Mu "x" (Cut (Lit 1) (Covar "x"))) (Lit 1) (Covar "a")
    Cut (Mu "x" (Cut (Lit 1) (Covar "x"))) (Mutilde "x0" (Op (Var "x0") Sum (Lit 1) (Covar "a")

    >>> focus (Op (Mu "x" (Cut (Lit 1) (Covar "x"))) (Mu "y" (Cut (Lit 1) (Covar "y"))) (Covar "a"))
    Cut (Mu "x" (Cut (Lit 1) (Covar "x"))) (MuTilde "x0" (Op (Mu "y" (Cut (Lit 1) (Covar "y"))) (Covar "a")) Sum (Var "x0")))
     -}
    focus s@(Op p1 op p2 c)
        -- when both producers are already values, we only need to focus all arguments of the operation
        | isValue p1 && isValue p2 = Op (focus p1) op (focus p2) (focus c)
        -- when only p1 is a value, we replace p2 by a fresh variable v
        -- this variable is then bound by a mu-tilde abstraction, which is the consumer of the resulting cut
        -- the producer of that cut is p2 focused.
        -- this works as with producers and consumers, replacing v by p2 once it is evaluated to a value
        | isValue p1 = let v = freshVar [s] in Cut (focus p2) (MuTilde v (focus (Op p1 op (Var v) c)))
        -- when both producer arguemtns are not values, we do the same as in the last case, but with p1 instead of p2
        | otherwise = let v = freshVar [s] in Cut (focus p1) (MuTilde v (focus (Op (Var v) op p2 c)))
    -- an ifzero statement works the same as a binary operation, just with only a single producer argument
    {-
     >>> focus (IfZ (Lit 1) Done Done)
     IfZ (Lit 1) Done Done

     >>> focus (IfZ (Mu "x" (Cut (Lit 1) (Covar "x"))) Done Done
     Cut (Mu "x" (Cut (Lit 1) (Covar "x"))) (MuTilde "x0" (IfZ (Var "x0") Done Done))
     -}
    focus s@(IfZ p s1 s2)
        -- when the producer is a value, we have to focus all arguments
        | isValue p = IfZ (focus p) (focus s1) (focus s2)
        -- when the producer is not a value, it is replaced by a new variable v bound by a mu-tilde abstraction
        -- this abstraction is placed in a cut with p as producer just as above
        | otherwise = let v = freshVar [s] in Cut (focus p) (MuTilde v (focus (IfZ (Var v) s1 s2)))
    -- top-level definitions are focused similarly to constructors and destructors
    {-
    >>> focus (Fun "Exit" [] [))
    (Fun "Exit" [] [])

    >>> focus (Fun "Succ" [Mu "x" (Cut (Lit 1) (Covar "x"))] [])
    (Cut (Mu "x" (Cut (Lit 1) (Covar "x"))) (MuTilde "x0" (Fun "Succ" [Var "x0"] [])))
     -}
    focus s@(Fun nm pargs cargs) =
        -- first find the first non-value producer argument
        case find (not . isValue) pargs of
            -- when there is no such argument, focus all arguments
            Nothing -> Fun nm (focus <$> pargs) (focus <$> cargs)
            -- otherwise replace p1 by a new variable
            Just p1 -> do
                -- generate variable to replace p1 by
                let v = freshVar [s]
                -- replace p1 by v in pargs
                let newArgs = (\p -> if p == p1 then Var v else p1) <$> pargs
                -- place p1 in a cut with mu-tilde abstraction whose producer is the toplevel function call
                -- but with argument p1 replaced by v
                Cut (focus p1) (MuTilde v (Fun nm newArgs cargs))
    {-
    >>> focus Done
    Done
     -}
    focus Done = Done

{- | Focusing instance for toplevel definitions
Focuses the defined body of the definition
-}
instance Focus (Def a) where
    {-
    >>> focus Def "Succ" ["x"] ["a"] (Op (Var "x") Sum (Lit 1) (Covar "a"))
    Def "Succ" ["x"] ["a"] (Op (Var "x") Sum (Lit 1) (Covar "a"))
    -}
    focus Def{name = nm, pargs = prods, cargs = cons, body = bd} = Def{name = nm, pargs = prods, cargs = cons, body = focus bd}

{- | Focusing instance for program
This focuses all definitions found in the program
-}
instance Focus (Program a) where
    {-
     >>> focus (MkProg [])
     MkProg []
    -}
    focus (MkProg dfs) = MkProg (focus <$> dfs)
