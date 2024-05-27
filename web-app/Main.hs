module Main where

import GHC.JS.Prim
import Compiler qualified as C
import Core.Pretty (render)
import Core.Eval

import JSBits
import Data.Text qualified as T
import GHC.JS.Foreign.Callback
import Duality.Parser
import Core.Focusing
import Core.Simplify
import Core.Syntax qualified as Core


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
  case parseProg (T.pack input) of
    Left err -> do
      setInputInvalid
      setErrorpane err
    Right prog -> do
      setInputValid
      let compiled = C.compileProg prog
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
showTrace steps = unlines $ map f (zip steps [(0::Integer)..])
  where
    f (stmt, i) = show i <> ": " <> render stmt
