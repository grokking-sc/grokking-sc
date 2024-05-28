module Core.Focusing where

import Core.Substitution
import Core.Syntax
import Data.List (find)

isValue :: Producer -> Bool
isValue (Lit _) = True
isValue (Var _) = True
isValue (Cocase _) = True
isValue (Constructor _ prds _) = all isValue prds
isValue (Mu _ _) = False


-- section 3.2
-- Focusing is needed for
-- producers
-- consumers
-- statements
-- patterns
-- definitions
-- programs
class Focus a where
    focus :: a -> a

-- for a pattern, the right-hand command needs to be focused,
-- everything else stays the same
instance Focus (Pattern a) where
    focus :: Pattern a -> Pattern a
    focus (MkPattern nm v cv s) = MkPattern nm v cv (focus s)

instance Focus Producer where
    focus :: Producer -> Producer
    -- variables are already focused as they are in normal form
    focus (Var x) = Var x
    -- literals are already focused as they are in normal form
    focus (Lit n) = Lit n
    -- for a mu abstracition, the contained command needs to be focused
    focus (Mu x s) = Mu x (focus s)
    -- in a cocase, the patterns have to be focused as above
    focus (Cocase cocases) = Cocase (focus <$> cocases)
    -- in a constructor term, focusing amounts to replacing the first non-value argument by a variable
    -- this way, that argument is evaluated first and then the variable is substituted by the evaluated argument
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
                let newArgs = (\p -> if p == p1' then Var v else p) <$> pargs
                -- the result is a mu abstraction with cv as variable
                -- this is needed, so the result is still a producer
                -- the statement bound by mu has producer p1' (after focusing p1' again)
                -- the consumer of this statement is a mutilde abstraction
                -- this means evaluating will then replace v with p1' (after p1' has been evaluated to a value
                -- the statment of that mutilde expression has producer that is the original constructor with v instead of p1'
                -- its consumer is the bound covariable bound by the outer mu abstraction
                -- this covariable acts as a continuatiion, allowing all to be evaluated in the correct order
                Mu cv (Cut (focus p1') (MuTilde v (Cut (focus (Constructor ct newArgs cargs)) (Covar cv))))

instance Focus Consumer where
    focus :: Consumer -> Consumer
    -- covariables are in normal form, so they are already focused
    focus (Covar x) = Covar x
    -- mutilde abstractions need the bound statement to be focused
    focus (MuTilde x s) = MuTilde x (focus s)
    -- as with cocases, cases need their patterns focused as above
    focus (Case cases) = Case (focus <$> cases)
    -- focusing a destructor term is analogous to focusing a constructor term
    -- we find the first non-value producer argument and replace it by a variable v
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

instance Focus Statement where
    focus :: Statement -> Statement
    -- in a cut we need to focus both the producer and consumer
    focus (Cut p c) = Cut (focus p) (focus c)
    -- in a binary operation focusing depends on which arguments are values
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
    focus s@(IfZ p s1 s2)
        -- when the producer is a value, we have to focus all arguments
        | isValue p = IfZ (focus p) (focus s1) (focus s2)
        -- when the producer is not a value, it is replaced by a new variable v bound by a mu-tilde abstraction
        -- this abstraction is placed in a cut with p as producer just as above
        | otherwise = let v = freshVar [s] in Cut (focus p) (MuTilde v (focus (IfZ (Var v) s1 s2)))
    -- top-level definitions are focused similarly to constructors and destructors
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
    -- Done is already in normal form, so already focused
    focus Done = Done

-- in a toplevel definition, focus the body
instance Focus (Def a) where
    focus Def{name = nm, pargs = prods, cargs = cons, body = bd} = Def{name = nm, pargs = prods, cargs = cons, body = focus bd}

-- in a program, focus all definitions
-- these definitions all need to be focused as well, as otherwise replacing a call by its definition might mean evaluation will be stuck
instance Focus (Prog a) where
    focus (MkProg dfs) = MkProg (focus <$> dfs)
