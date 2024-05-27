module Main (main) where

import Compiler qualified as C
import Duality.Syntax
import Duality.Parser
import Duality.Types
import Core.Focusing
import Core.Syntax qualified as Core
import Core.Eval

import Data.Text.IO qualified as T
import Core.Pretty (render)
import System.Exit (exitFailure)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    dispatch args

colorDefault :: String
colorDefault = "\ESC[0m"

colorTarget :: String
colorTarget = "\ESC[93m"

dispatch :: [String] -> IO ()
dispatch [fp] = do
    prog <- readAndParse fp

    tyRes <- inferTypes prog

    case tyRes of
        Left err -> putStrLn err >> exitFailure
        Right _ -> putStrLn "Program typechecks!"

    let compiled = C.compileProg prog
    putStrLn $ colorTarget <> "---------- Result of Compilation --------" <> colorDefault
    putStrLn (render compiled)

    let focused = focus compiled
    putStrLn $ colorTarget <> "---------- Result of Focusing --------" <> colorDefault
    putStrLn (render focused)

    let result = evalMain focused
    case result of
        Nothing -> do
            putStrLn "Main not found"
            exitFailure
        Just res -> do
          putStrLn $ colorTarget <> "---------- Result of Evaluation --------" <> colorDefault
          printTrace res
dispatch _ = putStrLn "Please invoke the program with a filepath"

printTrace :: [Core.Statement] -> IO ()
printTrace xs = go (zip xs [(0::Integer)..])
  where
    go [] = pure ()
    go ((s,i):rest) = do
        putStrLn (show i <> ": " <> render s)
        go rest

readAndParse :: FilePath -> IO (Prog ())
readAndParse fp = do
    file <- T.readFile fp
    case parseProg file of
        Left err -> do
            putStrLn err
            exitFailure
        Right prog -> pure prog
