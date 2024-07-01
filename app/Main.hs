module Main (main) where

import Compiler qualified as C
import Core.Eval
import Core.Focusing
import Core.Pretty (render)
import Core.Simplify
import Core.Syntax qualified as Core
import Data.Text.IO qualified as T
import Fun.Parser
import Fun.Syntax
import Fun.Types
import System.Environment (getArgs)
import System.Exit (exitFailure)

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

    case inferTypes prog of
        Left err -> putStrLn err >> exitFailure
        Right prog' -> do
            putStrLn $ colorTarget <> "---------- Result of Type Checking --------" <> colorDefault
            putStrLn (showTypedProg prog')

    let compiled = C.compileProgram prog
    putStrLn $ colorTarget <> "---------- Result of Compilation --------" <> colorDefault
    putStrLn (render compiled)

    let focused = focus compiled
    putStrLn $ colorTarget <> "---------- Result of Focusing --------" <> colorDefault
    putStrLn (render focused)

    let simplified = simplify focused
    putStrLn $ colorTarget <> "---------- Result of Simplification --------" <> colorDefault
    putStrLn (render simplified)

    let result = evalMain simplified
    case result of
        Nothing -> do
            putStrLn "Main not found"
            exitFailure
        Just res -> do
            putStrLn $ colorTarget <> "---------- Result of Evaluation --------" <> colorDefault
            printTrace res
dispatch _ = putStrLn "Please invoke the program with a filepath"

printTrace :: [Core.Statement] -> IO ()
printTrace xs = go (zip xs [(0 :: Integer) ..])
  where
    go [] = pure ()
    go ((s, i) : rest) = do
        putStrLn (show i <> ": " <> render s)
        go rest

readAndParse :: FilePath -> IO (Program ())
readAndParse fp = do
    file <- T.readFile fp
    case parseProgram file of
        Left err -> do
            putStrLn err
            exitFailure
        Right prog -> pure prog
