module Main where

import Parser (parserTests)
import Substitution (substitutionTests)
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, substitutionTests]
