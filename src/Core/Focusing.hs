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
import Data.Set qualified as S

{- | Test if a producer is a value.
The only non-values in CBV are mu-abstractions or
constructors applied to non-values.
-}
isValue :: Producer -> Bool
isValue (Lit _) = True
isValue (Var _) = True
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
isValue (MuDyn _ _) = False

{- | Type class for focusing
Definition 3.2
-}
class Focus a where
    -- | Focus
    focus :: a -> a

instance Focus (Pattern a) where
    focus :: Pattern a -> Pattern a
    {-
    >>> focus (MkPattern Nil [] [] Done)
    MkPattern Nil [] [] Done
    -}
    focus (MkPattern nm v cv s) = MkPattern nm v cv (focus s)

instance Focus Producer where
    focus :: Producer -> Producer
    --
    -- F("x") = "x"
    --
    focus (Var x) = Var x
    --
    -- F(n) = n
    --
    focus (Lit n) = Lit n
    --
    -- F(μ a.s) = μ a.F(s)
    --
    focus (Mu x s) = Mu x (focus s)
    focus (MuDyn x s) = MuDyn x (focus s)
    --
    -- F(cocase { D(xs;as) ⇒ s,... } = cocase { D(xs;as) ⇒ F(x), ...}
    --
    focus (Cocase cocases) = Cocase (focus <$> cocases)
    --
    -- F(K(ps,p,ps'cs)) = μ a.⟨F(p) | μ~ x.⟨F(K(ps,x,ps';cs)) | a⟩
    -- where ps are all values and p is the first non-value argument
    --
    focus cont@(Constructor ct pargs cargs) =
        case find (not . isValue) pargs of
            Nothing -> Constructor ct (focus <$> pargs) (focus <$> cargs)
            Just p -> do
                let v = freshVar [cont]
                let cv = freshCovar [cont]
                let newArgs = (\p' -> if p' == p then Var v else p') <$> pargs
                Mu cv (Cut (focus p) (MuTilde v (Cut (focus (Constructor ct newArgs cargs)) (Covar cv))))

instance Focus Consumer where
    focus :: Consumer -> Consumer
    --
    -- F(a) = a
    --
    focus (Covar x) = Covar x
    --
    -- F(μ~ x.s) = μ~ x.F(s)
    --
    focus (MuTilde x s) = MuTilde x (focus s)
    focus (MuTildeDyn x s) = MuTildeDyn x (focus s)
    --
    -- F(case { K(xs;as) ⇒ s, ... }) = case { K(xs;as) ⇒ F(s), ...}
    --
    focus (Case cases) = Case (focus <$> cases)
    --
    -- F(D(ps,p,ps';cs)) = μ~ y. ⟨F(p) | μ~ x.⟨y | F(D(ps,x,ps';cs))⟩⟩
    -- where ps are all values and p is the first non-value argument
    --
    focus dest@(Destructor dt pargs cargs) =
        case find (not . isValue) pargs of
            Nothing -> Destructor dt (focus <$> pargs) (focus <$> cargs)
            Just p -> do
                let v1 = freshVar [dest]
                let v2 = freshVarFrom [dest] (S.singleton v1)
                let newArgs = (\p' -> if p' == p then Var v1 else p') <$> pargs
                MuTilde v2 (Cut (focus p) (MuTilde v1 (Cut (Var v2) (focus (Destructor dt newArgs cargs)))))

instance Focus Statement where
    focus :: Statement -> Statement
    --
    -- F(⟨p | c⟩) = ⟨F(p) | F(c) ⟩
    --
    focus (Cut p c) = Cut (focus p) (focus c)
    --
    -- F(⊙(p1,p2;c) = ⟨n⊙m | c⟩ = ⊙(F(p1),F(p2);F(c))
    -- if both @p1@ and @p2@ are values.
    --
    -- F(⊙(p1,p2;c) = ⟨ F(p2) |  μ~ x. F(⊙(p1,x;c))⟩
    -- if @p1@ is a value but @p2@ is not
    --
    -- F(⊙(p1,p2;c) = ⟨ F(p1) |  μ~ x. F(⊙(x,p2;c))⟩
    -- if @p1@ is not a value
    --
    focus s@(Op p1 op p2 c)
        | isValue p1 && isValue p2 = Op (focus p1) op (focus p2) (focus c)
        | isValue p1 = let v = freshVar [s] in Cut (focus p2) (MuTilde v (focus (Op p1 op (Var v) c)))
        | otherwise = let v = freshVar [s] in Cut (focus p1) (MuTilde v (focus (Op (Var v) op p2 c)))
    --
    -- F(ifz(p;s1,s2)) = ⟨ F(p) | μ~ x.ifz(x;s1,s2)⟩
    -- if p is not a value
    --
    -- F(ifz(p;s1,s2)) = ifz(F(p);F(s1),F(s2))
    -- if p is a value
    --
    focus s@(IfZ p s1 s2)
        | isValue p = IfZ (focus p) (focus s1) (focus s2)
        | otherwise = let v = freshVar [s] in Cut (focus p) (MuTilde v (focus (IfZ (Var v) s1 s2)))
    --
    -- F(f(ps,p,ps';cs)) = ⟨ F(p) | μ~ x.F(f(ps,x,ps';cs))⟩
    -- if @ps@ are all values and @p@ is the first non-value argument
    --
    -- F(f(ps;cs)) = f(F(ps);F(cs))
    -- if all producer arguments are values
    --
    focus s@(Fun nm pargs cargs) =
        case find (not . isValue) pargs of
            Nothing -> Fun nm (focus <$> pargs) (focus <$> cargs)
            Just p1 -> do
                let v = freshVar [s]
                let newArgs = (\p -> if p == p1 then Var v else p) <$> pargs
                Cut (focus p1) (MuTilde v (focus (Fun nm newArgs cargs)))
    focus Done = Done

instance Focus (Def a) where
    {-
    >>> focus Def "Succ" ["x"] ["a"] (Op (Var "x") Sum (Lit 1) (Covar "a"))
    Def "Succ" ["x"] ["a"] (Op (Var "x") Sum (Lit 1) (Covar "a"))
    -}
    focus Def{name = nm, pargs = prods, cargs = cons, body = bd} = Def{name = nm, pargs = prods, cargs = cons, body = focus bd}

instance Focus (Program a) where
    focus (MkProg dfs) = MkProg (focus <$> dfs)
