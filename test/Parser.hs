{-# LANGUAGE OverloadedStrings #-}

module Parser (parserTests) where

import Data.Text (Text)
import Data.Text qualified as T
import Fun.Parser
import Fun.Syntax
import Test.Tasty
import Test.Tasty.HUnit

parserTests :: TestTree
parserTests =
    testGroup
        "Parser"
        [ -- Term Tests
          testLit
        , testLitParens
        , testVar
        , testVarParens
        , testProd
        , testNil
        , testNilParens
        , testCons
        , testTup
        , testCaseTup
        , testCocaseStream
        , testThrow
        , testCatch
        , testIfz
        , testLet
        , testLambda
        , testFuncall
        , testFuncallParens
        , testFuncCont
        , testHd
        , testTl
        , testTlTl
        , testFst
        , testSnd
        , testApp1
        , testApp2
        , testOpPlus
        , testOpProd
        , testOpSub
        , testOpNested
        , -- Program tests
          simpleProg1
        , simpleProg2
        , simpleProg3
        , simpleProg4
        ]

-- Helper utilities

mkTermTest :: Text -> Term -> TestTree
mkTermTest str tm = testCase (T.unpack ("Expression \"" <> str <> "\" can be parsed")) $ parseTerm str @?= Right tm

mkProgTest :: Text -> Program () -> TestTree
mkProgTest str prog =
    testCase (T.unpack ("Programm \"" <> str <> "\" can be parsed")) $ parseProgram str @?= Right prog

-- Term Tests

testLit :: TestTree
testLit = mkTermTest "2" (Lit 2)

testLitParens :: TestTree
testLitParens = mkTermTest "(2)" (Lit 2)

testVar :: TestTree
testVar = mkTermTest "foo" (VarT "foo")

testVarParens :: TestTree
testVarParens = mkTermTest "(foo)" (VarT "foo")

testProd :: TestTree
testProd = mkTermTest "2 * 3" (Op (Lit 2) Prod (Lit 3))

testNil :: TestTree
testNil = mkTermTest "Nil" (Constructor Nil [])

testNilParens :: TestTree
testNilParens = mkTermTest "(Nil)" (Constructor Nil [])

testCons :: TestTree
testCons = mkTermTest "Cons(x,xs)" (Constructor Cons [VarT "x", VarT "xs"])

testTup :: TestTree
testTup = mkTermTest "Tup(2 , 3 )" (Constructor Tup [Lit 2, Lit 3])

testCaseTup :: TestTree
testCaseTup = mkTermTest "case x of { Tup(x,x) => x}" (Case (VarT "x") [MkClause Tup ["x", "x"] (VarT "x")])

testCocaseStream :: TestTree
testCocaseStream =
    mkTermTest
        "cocase { hd => x, tl => x}"
        (Cocase [MkClause Hd [] (VarT "x"), MkClause Tl [] (VarT "x")])

testThrow :: TestTree
testThrow = mkTermTest "goto(2;x)" (Goto (Lit 2) "x")

testCatch :: TestTree
testCatch = mkTermTest "label x { 2 }" (Label "x" (Lit 2))

testIfz :: TestTree
testIfz = mkTermTest "ifz(1,2,3) " (IfZ (Lit 1) (Lit 2) (Lit 3))

testLet :: TestTree
testLet = mkTermTest "let x = 2 in x" (Let "x" (Lit 2) (VarT "x"))

testLambda :: TestTree
testLambda = mkTermTest "\\x => x" (Lam "x" (VarT "x"))

testFuncall :: TestTree
testFuncall = mkTermTest "fac(10)" (Fun "fac" [Lit 10] Nothing)

testFuncallParens :: TestTree
testFuncallParens = mkTermTest "(fac(10))" (Fun "fac" [Lit 10] Nothing)

testFuncCont :: TestTree
testFuncCont =
    mkTermTest
        "fmult(Cons(1,Cons(0,Cons(2,Nil)));beta)"
        ( Fun
            "fmult"
            [Constructor Cons [Lit 1, Constructor Cons [Lit 0, Constructor Cons [Lit 2, Constructor Nil []]]]]
            (Just "beta")
        )

testHd :: TestTree
testHd = mkTermTest "x.hd" (Destructor (VarT "x") Hd [])

testTl :: TestTree
testTl = mkTermTest "x.tl" (Destructor (VarT "x") Tl [])

testTlTl :: TestTree
testTlTl = mkTermTest "x.tl.tl" (Destructor (Destructor (VarT "x") Tl []) Tl [])

testFst :: TestTree
testFst = mkTermTest "x.fst" (Destructor (VarT "x") Fst [])

testSnd :: TestTree
testSnd = mkTermTest "x.snd" (Destructor (VarT "x") Snd [])

testApp1 :: TestTree
testApp1 = mkTermTest "f x" (App (VarT "f") (VarT "x"))

testApp2 :: TestTree
testApp2 = mkTermTest "f x y" (App (App (VarT "f") (VarT "x")) (VarT "y"))

testOpPlus :: TestTree
testOpPlus = mkTermTest "2 + 3" (Op (Lit 2) Sum (Lit 3))

testOpProd :: TestTree
testOpProd = mkTermTest "(2 * 3)" (Op (Lit 2) Prod (Lit 3))

testOpSub :: TestTree
testOpSub = mkTermTest "(2 - 3)" (Op (Lit 2) Sub (Lit 3))

testOpNested :: TestTree
testOpNested = mkTermTest "(2 * 3) + (4 - 1)" (Op (Op (Lit 2) Prod (Lit 3)) Sum (Op (Lit 4) Sub (Lit 1)))

-- Program tests

simpleProg1 :: TestTree
simpleProg1 = mkProgTest "def foo := x;" (MkProg [Def "foo" [] Nothing (VarT "x") ()])

simpleProg2 :: TestTree
simpleProg2 = mkProgTest "def foo(x) := x;" (MkProg [Def "foo" [("x", ())] Nothing (VarT "x") ()])

simpleProg3 :: TestTree
simpleProg3 =
    mkProgTest
        "def foo(x) := x; def bar(y) := y;"
        (MkProg [Def "foo" [("x", ())] Nothing (VarT "x") (), Def "bar" [("y", ())] Nothing (VarT "y") ()])

simpleProg4 :: TestTree
simpleProg4 =
    mkProgTest
        "def mult(l) := label a { mult2(l;a)}; def mult2(l;a) := case l of { Nil => 1,Cons(x,xs) => ifz(x,goto(0;a),x*mult2(xs;a))};"
        ( MkProg
            [ Def "mult" [("l", ())] Nothing (Label "a" (Fun "mult2" [VarT "l"] (Just "a"))) ()
            , Def
                "mult2"
                [("l", ())]
                (Just "a")
                ( Case
                    (VarT "l")
                    [ MkClause Nil [] (Lit 1)
                    , MkClause
                        Cons
                        ["x", "xs"]
                        (IfZ (VarT "x") (Goto (Lit 0) "a") (Op (VarT "x") Prod (Fun "mult2" [VarT "xs"] (Just "a"))))
                    ]
                )
                ()
            ]
        )
