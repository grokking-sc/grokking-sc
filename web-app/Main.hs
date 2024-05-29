module Main where

import Compiler qualified as C
import Core.Eval
import Core.Focusing
import Core.Pretty (render)
import Core.Simplify
import Core.Syntax qualified as Core
import Data.Text qualified as T
import Fun.Parser
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import JSBits

-------------------------------------------------------------------------------
-- Helper function to set global variable "globalCompiler"
-------------------------------------------------------------------------------

-- Cp. https://downloads.haskell.org/ghc/latest/docs/users_guide/javascript.html#callbacks-as-foreign-exports
foreign import javascript "((compiler) => { globalCompiler = compiler })"
    setCompiler :: Callback (JSVal -> IO ()) -> IO ()

-- | Clear all textareas
clearAll :: IO ()
clearAll = do
    setErrorpane " "
    setCompiled " "
    setFocused " "
    setSimplified " "
    setEvaluation " "

compiler :: JSVal -> IO ()
compiler val = do
    let input = fromJSString val
    clearAll
    case parseProgram (T.pack input) of
        Left err -> do
            setInputInvalid
            setErrorpane err
        Right prog -> do
            setInputValid
            let compiled = C.compileProgram prog
            setCompiled (render compiled)
            let focused = focus compiled
            setFocused (render focused)
            let simplified = simplify focused
            setSimplified (render simplified)

            let evalRes = evalMain simplified
            case evalRes of
                Nothing -> do
                    setEvaluation "Definition \"main\" not found."
                Just trace -> do
                    setEvaluation (showTrace trace)

createCompiler :: IO (Callback (JSVal -> IO ()))
createCompiler = syncCallback1 ThrowWouldBlock compiler

main :: IO ()
main = do
    cp <- createCompiler
    setCompiler cp

showTrace :: [Core.Statement] -> String
showTrace steps = unlines $ map f (zip steps [(0 :: Integer) ..])
  where
    f (stmt, i) = show i <> ": " <> render stmt
