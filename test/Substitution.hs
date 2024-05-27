module Substitution (substitutionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Core.Syntax
import Core.Substitution
import Core.Pretty
import Duality.Syntax (Ctor(..))
import Prettyprinter

substitutionTests :: TestTree
substitutionTests = testGroup "Substitution" [subst1, subst2, subst3, subst4, subst5, subst6, subst7, subst8, subst9, subst10]

-- Test substVar:
mkVarTest :: (Eq a, Show a, Subst a, Pretty a) => a -> Producer -> Var -> a -> TestTree
mkVarTest x p v expected =
    testCase (render x <> "[" <> render p <> "/" <> render v <> "] = " <> render expected) $ substVar p v x @?= expected

mkCovarTest :: (Eq a, Show a, Subst a, Pretty a) => a -> Consumer -> Covar -> a -> TestTree
mkCovarTest x p v expected =
    testCase (render x <> "[" <> render p <> "/" <> render v <> "] = " <> render expected) $ substCovar p v x @?= expected

-- Relatively simple testcases

subst1 :: TestTree
subst1 = mkVarTest (Var "x") (Lit 2) "x" (Lit 2)

subst2 :: TestTree
subst2 = mkVarTest (Var "y") (Lit 2) "x" (Var "y")

subst3 :: TestTree
subst3 = mkVarTest (Cut (Var "x") (Covar "a")) (Lit 2) "x" (Cut (Lit 2) (Covar "a"))

subst4 :: TestTree
subst4 = mkVarTest (Mu "a" (Cut (Var "x") (Covar "a"))) (Lit 2) "x" (Mu "a0" (Cut (Lit 2) (Covar "a0")))

subst5 :: TestTree
subst5 = mkVarTest (Mu "a" (Cut (Var "x") (Covar "a"))) (Var "x") "x" (Mu "a0" (Cut (Var "x") (Covar "a0")))

-- Covar tests
subst6 :: TestTree
subst6 = mkCovarTest (Mu "a0" (Cut (Var "x") (Covar "a0"))) (Covar "b") "c" (Mu "a1" (Cut (Var "x") (Covar "a1")))

subst7 :: TestTree
subst7 = mkCovarTest (Mu "a0" (Cut (Var "x") (Covar "a0"))) (Covar "a0") "c" (Mu "a1" (Cut (Var "x") (Covar "a1")))

subst8 :: TestTree
subst8 = mkCovarTest (Mu "a0" (Cut (Var "x") (Covar "a0"))) (Covar "b") "a0" (Mu "a1" (Cut (Var "x") (Covar "a1")))

subst9 :: TestTree
subst9 = mkCovarTest (MuTilde "x" (Cut (Var "x") (Covar "a"))) (Covar "b") "a" (MuTilde "x0" (Cut (Var "x0") (Covar "b")))

-- More complicated testcases
-- 〈 μa0. 〈 x | case {Nil ⇒ 〈 μa0. 〈 0 | a 〉 | a0 〉} 〉 | a 〉 [a0/a]
subst10 :: TestTree
subst10 = mkCovarTest input (Covar "a0") "a" expected
  where
    input    = (Cut (Mu "a0" (Cut (Var "x") (Case [MkPattern Nil [] [] (Cut (Mu "a0" (Cut (Lit 0) (Covar "a"))) (Covar "a0"))]))) (Covar "a"))
    expected = (Cut (Mu "a1" (Cut (Var "x") (Case [MkPattern Nil [] [] (Cut (Mu "a1" (Cut (Lit 0) (Covar "a0"))) (Covar "a1"))]))) (Covar "a0"))


