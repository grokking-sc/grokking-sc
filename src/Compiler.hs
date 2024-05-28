{- |
Module      : Compiler
Description : Compilation of the surface language @Fun@ to the intermediate language @Core@.

This module implements the compilation of the surface language Fun to the intermediate
language Core. The compilation algorithm is explained in definitions 2.1 to 2.6 of the
paper.
-}
module Compiler (compile, compileProgram) where

import Core.Substitution (freshCovar)
import Core.Syntax qualified as Core
import Fun.Syntax qualified as Fun

{- | Compile a term of the surface language @Fun@ to a producer of the intermediate
language @Core.
-}
compile :: Fun.Term -> Core.Producer
-- section 2.1
-- variable and literal terms appear in both core and duality
compile (Fun.VarT v) = Core.Var v
compile (Fun.Lit n) = Core.Lit n
-- binary operations in core are statements while they are terms in duality
compile (Fun.Op t1 op t2) = do
    -- compile the (producer) arguments
    let (t1', t2') = (compile t1, compile t2)
    -- add a covariable that does not appear in t1' and t2'
    -- this acts as the continuation for the resuliting core prducer
    let cv = freshCovar [t1', t2']
    -- t1' op and t2' can be directly used for core.op
    -- we also add cv as continuation and a mu abstraction to turn the term into a producer
    Core.Mu cv (Core.Op t1' op t2' (Core.Covar cv))
-- ifz are statements in core
compile (Fun.IfZ t1 t2 t3) = do
    -- first compile (producer) arguments
    let (t1', t2', t3') = (compile t1, compile t2, compile t3)
    -- generate a new covariable not appearing in the arguments
    let cv = freshCovar [t1', t2', t3']
    -- to turn the two branches into statements they are cut with the new covariable
    let s1 = Core.Cut t2' (Core.Covar cv)
    let s2 = Core.Cut t3' (Core.Covar cv)
    -- then the producer argument of ifz in core is the first argument in duality
    -- the two statements with continuation cv are the two branches in coer
    -- and we again add a mu abstraction around the ifz-statement
    Core.Mu cv (Core.IfZ t1' s1 s2)

-- Section 2.2
-- let-bindings are compiled to mu-tilde absractions
compile (Fun.Let v t1 t2) = do
    -- first compile the two terms
    let (t1', t2') = (compile t1, compile t2)
    -- generate a continuation covariable
    let cv = freshCovar [t1', t2']
    -- this covariable is the continuation for the right-hand side of the let-biding
    let s' = Core.Cut t2' (Core.Covar cv)
    -- v is the bound variable of a mutilde abstraction
    -- cv is the bound variable of a mu abstraction
    -- the mutilde abstraction is cut with t1' so t1' will be substituted for v during evaluation
    -- and we add the outer mu abstraction as before
    Core.Mu cv (Core.Cut t1' (Core.MuTilde v s'))

-- section 2.3
compile (Fun.Fun v args Nothing) = do
    -- arguments can be directly compiled and will be the producer arguments
    let args' = compile <$> args
    -- wee need one extra covariable argument to act as a continuation
    let cv = freshCovar args'
    -- this covariable is then used in a mu-abstraction and the single consumer argument of the compiled function
    -- see compilation for definitions that this is in line with argument aritiies of compiled definitions
    Core.Mu cv (Core.Fun v args' [Core.Covar cv])
compile (Fun.Fun v args (Just cv)) = do
    -- when a toplevel call has a covariable argument (i.e. a label) we do the same as before
    -- but now the final toplevel call has two consumer arguments instead of one
    let args' = compile <$> args
    let cv' = freshCovar args'
    Core.Mu cv (Core.Fun v args' [Core.Covar cv', Core.Covar cv])

-- section 2.4
compile (Fun.ConT ct ctargs) = do
    -- a constructor term can be directly translated by translating its arguments
    -- the core constructor will have no consumer arguments
    Core.Constructor ct (compile <$> ctargs) []
compile (Fun.DesT t dt args) = do
    -- in core destructors have no scrutinee terms
    -- thus the compiled scrutinee t' is used as the producer of a cut
    -- the consumer of that cut is the compiled destructor term
    -- since in core destructors are consumers
    -- to turn everything into a producer we again use a fresh covariable with a mu abstraction
    let t' = compile t
    let cv = freshCovar [t']
    let args' = compile <$> args
    let newD = Core.Destructor dt args' [Core.Covar cv]
    Core.Mu cv (Core.Cut t' newD)
compile (Fun.Case t pts) = do
    -- as with destructors, cases in core are consumers and have no scrutinee
    -- thus we again use t' as producer of a cut with the compiled case as consumer
    -- and we again use a mu abstraction with fresh covariable to turn it into a producer
    let t' = compile t
    -- to compile the patterns, we compile all right-hand sides
    -- these will be producers, but need to be statements in core
    -- thus we all cut them with the freshly generated covariable
    -- this covariable then acts as continuation
    -- the final core pattern then has the same form as the duality pattern with no consumer arguments
    let rhss = compile . (\(Fun.MkClause _ _ t1) -> t1) <$> pts
    let cv = freshCovar (t' : rhss)
    let rhss' = (\p -> Core.Cut p (Core.Covar cv)) <$> rhss
    let ptFun (Fun.MkClause ct v _, c) = Core.MkPattern ct v [] c
    let pts' = ptFun <$> zip pts rhss'
    Core.Mu cv (Core.Cut t' (Core.Case pts'))
compile (Fun.Cocase patterns) = do
    -- to compile cocases we only need to compile all patterns
    Core.Cocase (compilePattern <$> patterns)
  where
    compilePattern :: Fun.Clause Fun.Dtor -> Core.Pattern Fun.Dtor
    compilePattern (Fun.MkClause dt vars t) = do
        -- compiling a pattern works as with cases
        -- first the right hand side is compiled
        -- then a new covariable is generated to act as continuation
        -- the compiled producer is cut with that covariable to get a statemnt
        -- however, now the generated covariable needs to be added to the consumer arguments of the pattern
        -- this is because cocases are producers
        -- and the continuation depends on the consumer (i.e destructor) that will consumer the copattern
        let t' = compile t
        let cv = freshCovar [t']
        let newCut = Core.Cut t' (Core.Covar cv)
        Core.MkPattern dt vars [cv] newCut

-- section 2.5
-- labmda abstractions are compiled to cocases of ap
compile (Fun.Lam v t) = do
    -- the resulting cocase has a single pattern for the destructor Ap
    -- the single produer argument is the bound variable
    -- and the consumer argument is the generated covariable
    -- its statement is the compiled body cut with the generated covariable
    -- note that this is the same as compiling the duality cocase
    -- cocase { Ap(v) => t }
    let t' = compile t
    let cv = freshCovar [t']
    Core.Cocase
        [ Core.MkPattern
            { Core.xtor = Fun.Ap
            , Core.patv = [v]
            , Core.patcv = [cv]
            , Core.patst = Core.Cut t' (Core.Covar cv)
            }
        ]
-- a function application is compiled to a destructor cut with a function
-- if t1 is a function (i.e. lambda abstraction) it will be compiled to a cocase
-- then evaluation works in core as in duality
-- this compilation is the same as compiling the duality term t1.Ap(t2)
compile (Fun.App t1 t2) = do
    let (t1', t2') = (compile t1, compile t2)
    let cv = freshCovar [t1', t2']
    let newD = Core.Destructor Fun.Ap [t2'] [Core.Covar cv]
    Core.Mu cv (Core.Cut t1' newD)

-- section 2.6
-- Goto is compiled to a mu abstraction
-- note that in contrast to other compilations, here cv' is generated but not used in the cut
-- instead we use the label cv (which is already a covariable)
compile (Fun.Goto t cv) = do
    let t' = compile t
    let cv' = freshCovar [t']
    Core.Mu cv' (Core.Cut t' (Core.Covar cv))
-- labels are translated to mu abstractions as well
-- here, the bound covariable is the same as the label
compile (Fun.Label cv t) = do
    Core.Mu cv (Core.Cut (compile t) (Core.Covar cv))

-- | Compile a single definition of the surface language @Fun@ to the intermediate language @Core@.
compileDef :: Fun.Def a -> Core.Def a
compileDef (Fun.Def nm prodargs Nothing bd rt) = do
    -- compile the body and generate a new covariable
    let bd' = compile bd
    let cv = freshCovar [bd']
    -- the compiled producer term is cut with the new covariable
    -- this way the body of the compiled definition is a statement
    -- the covariable is used as continuation, so it is added to the consumer arguments of the definition
    -- since it is the continuation its type is the same as the return type
    let newCut = Core.Cut bd' (Core.Covar cv)
    Core.Def nm prodargs [(cv, rt)] newCut
compileDef (Fun.Def nm prodargs (Just cv) bd rt) = do
    -- if the definition already has a consumer argument compiliation proceeds the same way
    -- we now just have two consumer arguments instead of 1
    -- since both of these act as continuations they both have the same return type which is the return type of the function
    let bd' = compile bd
    let cv' = freshCovar [bd']
    let newCut = Core.Cut bd' (Core.Covar cv)
    Core.Def nm prodargs [(cv', rt), (cv, rt)] newCut

-- note that this corresponds to the translation of toplevel calls, which also adds a consumer argument

{- | Compile a program of the surface language @Fun@ to a program of the intermediate
language @Core.
-}
compileProgram :: Fun.Program a -> Core.Prog a
compileProgram (Fun.MkProg defs) = Core.MkProg (compileDef <$> defs)
