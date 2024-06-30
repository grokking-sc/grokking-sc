{- |
Module      : Compiler
Description : Compilation of the surface language @Fun@ to the intermediate language @Core@.

This module implements the compilation of the surface language Fun to the intermediate
language Core. The compilation algorithm is explained in definitions 2.1 to 2.6 of the
paper.
-}
module Compiler (compile, compileProgram) where

import Control.Monad.State
import Core.Substitution (Free (..), freshCovarFrom)
import Core.Syntax qualified as Core
import Data.Set qualified as S
import Fun.Syntax qualified as Fun

{- | Compile a term of the surface language @Fun@ to a producer of the intermediate
language @Core@.
-}
compile :: Fun.Term -> State (S.Set Core.Covar) Core.Producer
-- ⟦ x ⟧ = x
compile (Fun.VarT v) = pure (Core.Var v)
-- ⟦ ⌜n⌝ ⟧ = ⌜n⌝
compile (Fun.Lit n) = pure (Core.Lit n)
-- ⟦ t1 ☉ t2 ⟧ = µɑ. ☉(⟦ t1 ⟧, ⟦ t2 ⟧, ɑ)  (ɑ fresh)
compile (Fun.Op t1 op t2) = do
    t1' <- compile t1
    t2' <- compile t2
    usedCovars <- get
    let alpha = freshCovarFrom [t1', t2'] usedCovars
    put (usedCovars `S.union` S.singleton alpha)
    pure (Core.Mu alpha (Core.Op t1' op t2' (Core.Covar alpha)))
-- ⟦ ifz(t1,t2,t3) ⟧ = µɑ. ifz(⟦ t1 ⟧, ⟨ ⟦ t2 ⟧ | ɑ ⟩, ⟨ ⟦ t3 ⟧ | ɑ ⟩) (ɑ fresh)
compile (Fun.IfZ t1 t2 t3) = do
    t1' <- compile t1
    t2' <- compile t2
    t3' <- compile t3
    usedCovars <- get
    let alpha = freshCovarFrom [t1', t2', t3'] usedCovars
    put (usedCovars `S.union` S.singleton alpha)
    let s1 = Core.Cut t2' (Core.Covar alpha)
    let s2 = Core.Cut t3' (Core.Covar alpha)
    pure (Core.Mu alpha (Core.IfZ t1' s1 s2))
-- ⟦ let x = t1 in t2 ⟧ = µɑ. ⟨ ⟦ t1 ⟧ | ~µx. ⟨ ⟦ t2 ⟧ | ɑ ⟩⟩ (ɑ fresh)
compile (Fun.Let v t1 t2) = do
    t1' <- compile t1
    t2' <- compile t2
    usedCovars <- get
    let alpha = freshCovarFrom [t1', t2'] usedCovars
    put (usedCovars `S.union` S.singleton alpha)
    pure (Core.Mu alpha (Core.Cut t1' (Core.MuTilde v (Core.Cut t2' (Core.Covar alpha)))))
-- ⟦ f(t1,...,tn;ɑ1,...,ɑn) ⟧ = µɑ. f(⟦ t1 ⟧,...,⟦ tn ⟧;ɑ1,...,ɑn,ɑ) (ɑ fresh)
compile (Fun.Fun v args cvs) = do
    args' <- mapM compile args
    usedCovars <- get
    let alpha = freshCovarFrom [MkFree args', MkFree (Core.Covar <$> cvs)] usedCovars
    put (usedCovars `S.union` S.singleton alpha)
    pure (Core.Mu alpha (Core.Fun v args' ((Core.Covar <$> cvs) ++ [Core.Covar alpha])))
-- ⟦ K(t1,...,tn) ⟧ = K(⟦ t1 ⟧,...,⟦ tn ⟧)
compile (Fun.Constructor ct ctargs) = do
    ctargs' <- mapM compile ctargs
    pure (Core.Constructor ct ctargs' [])
-- ⟦ t.D(t1,...,tn) ⟧ = µɑ. ⟨ ⟦ t ⟧ | D(⟦ t1 ⟧,...,⟦ tn ⟧;ɑ) ⟩ (ɑ fresh)
compile (Fun.Destructor t dt args) = do
    t' <- compile t
    args' <- mapM compile args
    usedCovars <- get
    let alpha = freshCovarFrom (MkFree t' : (MkFree <$> args')) usedCovars
    put (usedCovars `S.union` S.singleton alpha)
    pure (Core.Mu alpha (Core.Cut t' (Core.Destructor dt args' [Core.Covar alpha])))
-- ⟦ case ts of { K(x1,...,xn) ⇒ t , ...} ⟧ = µɑ. ⟨ ⟦ ts ⟧ | case  {K(x1,...,xn) ⇒ ⟨ ⟦ t ⟧ | ɑ ⟩ ,...} ⟩ (ɑ fresh)
compile (Fun.Case ts clauses) = do
    ts' <- compile ts
    -- The following code is slightly awkward. This is due to the fact that @alpha@ has to be fresh for all
    -- of the terms on the right-hand side of clauses, but we must first compile them to @Core@ before we can
    -- compute the fresh variable @alpha@.
    rhss <- mapM (compile . (\(Fun.MkClause _ _ t1) -> t1)) clauses
    usedCovars <- get
    let alpha = freshCovarFrom (ts' : rhss) usedCovars
    put (usedCovars `S.union` S.singleton alpha)
    let rhss' = (\p -> Core.Cut p (Core.Covar alpha)) <$> rhss
    let ptFun (Fun.MkClause ct v _, c) = Core.MkPattern ct v [] c
    let pts' = ptFun <$> zip clauses rhss'
    pure (Core.Mu alpha (Core.Cut ts' (Core.Case pts')))
-- ⟦ cocase { D(x1...xn) ⇒ t, ...} ⟧ = cocase { D(x1...xn;ɑ) ⇒ ⟨ ⟦ t ⟧ | ɑ ⟩ , ...} (ɑ fresh in each cocase)
compile (Fun.Cocase patterns) = do
    patterns' <- mapM compileCocase patterns
    pure (Core.Cocase patterns')
  where
    compileCocase :: Fun.Clause Fun.Dtor -> State (S.Set Core.Covar) (Core.Pattern Fun.Dtor)
    compileCocase (Fun.MkClause dt vars t) = do
        t' <- compile t
        usedCovars <- get
        let alpha = freshCovarFrom [t'] usedCovars
        put (usedCovars `S.union` S.singleton alpha)
        pure (Core.MkPattern dt vars [alpha] (Core.Cut t' (Core.Covar alpha)))
-- ⟦ λx.t ⟧ = cocase { ap(x,ɑ) ⇒ ⟨ ⟦ t ⟧ | ɑ ⟩ } (ɑ fresh)
compile (Fun.Lam x t) = do
    t' <- compile t
    usedCovars <- get
    let alpha = freshCovarFrom [t'] usedCovars
    put (usedCovars `S.union` S.singleton alpha)
    pure (
        Core.Cocase
            [Core.MkPattern Fun.Ap [x] [alpha] (Core.Cut t' (Core.Covar alpha))]
        )
-- ⟦ t1 t2 ⟧ = µɑ. ⟨ ⟦ t1 ⟧ | ap(⟦ t2 ⟧;ɑ) ⟩ (ɑ fresh)
compile (Fun.App t1 t2) = do
    t1' <- compile t1
    t2' <- compile t2
    usedCovars <- get
    let alpha = freshCovarFrom [t1', t2'] usedCovars
    put (usedCovars `S.union` S.singleton alpha)
    pure (Core.Mu alpha (Core.Cut t1' (Core.Destructor Fun.Ap [t2'] [Core.Covar alpha])))
-- ⟦ goto(t;ɑ) ⟧ = µβ. ⟨ ⟦ t ⟧ | ɑ ⟩ (β fresh)
compile (Fun.Goto t alpha) = do
    t' <- compile t
    usedCovars <- get
    let beta = freshCovarFrom [MkFree t'] (usedCovars `S.union` S.singleton alpha)
    put (usedCovars `S.union` S.singleton beta)
    pure (Core.Mu beta (Core.Cut t' (Core.Covar alpha)))
-- ⟦ label ɑ { t } ⟧ = µɑ. ⟨ ⟦ t ⟧ | ɑ ⟩
compile (Fun.Label alpha t) = do
    usedCovars <- get
    put (usedCovars `S.union` S.singleton alpha)
    t' <- compile t
    pure (Core.Mu alpha (Core.Cut t' (Core.Covar alpha)))

{- | Compile a single definition of the surface language @Fun@ to the intermediate language @Core@.
⟦ def f(x1,...xn;ɑ1,...,ɑm) := t ⟧ = def f(x1,...xn;ɑ1,...,ɑm,ɑ) := ⟨ ⟦ t ⟧ | ɑ ⟩ (ɑ fresh)
-}
compileDef :: Fun.Def a -> Core.Def a
compileDef (Fun.Def nm prodargs cvs bd rt) = do
    let (bd', usedCovars) = runState (compile bd) (S.fromList (fst <$> cvs))
    let cv' = freshCovarFrom [MkFree bd', MkFree (Core.Covar . fst <$> cvs)] usedCovars
    let newCut = Core.Cut bd' (Core.Covar cv')
    Core.Def nm prodargs (cvs ++ [(cv', rt)]) newCut

{- | Compile a program of the surface language @Fun@ to a program of the intermediate
language @Core@.
-}
compileProgram :: Fun.Program a -> Core.Program a
compileProgram (Fun.MkProg defs) = Core.MkProg (compileDef <$> defs)
