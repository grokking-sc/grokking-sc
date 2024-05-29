module JSBits (
    setCompiled,
    setFocused,
    setSimplified,
    setErrorpane,
    setEvaluation,
    setInputInvalid,
    setInputValid,
) where

import Foreign.C.String

-------------------------------------------------------------------------------
-- Helper functions to enable/disable aria-invalid of input box
-------------------------------------------------------------------------------

foreign import javascript "((x,y) => document.getElementById('input').setAttribute('aria-invalid','false'))"
    setInputValidInternal :: CString -> IO ()

setInputValid :: IO ()
setInputValid = withCString "x" setInputValidInternal

foreign import javascript "((x,y) => document.getElementById('input').setAttribute('aria-invalid','true'))"
    setInputInvalidInternal :: CString -> IO ()

setInputInvalid :: IO ()
setInputInvalid = withCString "x" setInputInvalidInternal

-------------------------------------------------------------------------------
-- Helper functions to set the content of the textboxes
-------------------------------------------------------------------------------

-- "id=errorpane"

foreign import javascript "((arr,offset) => document.getElementById('errorpane').value = h$decodeUtf8z(arr,offset))"
    setErrorpaneInternal :: CString -> IO ()

setErrorpane :: String -> IO ()
setErrorpane s = withCString s setErrorpaneInternal

-- "id=compiled"

foreign import javascript "((arr,offset) => document.getElementById('compiled').value = h$decodeUtf8z(arr,offset))"
    setCompiledInternal :: CString -> IO ()

setCompiled :: String -> IO ()
setCompiled s = withCString s setCompiledInternal

-- "id=focused"

foreign import javascript "((arr,offset) => document.getElementById('focused').value = h$decodeUtf8z(arr,offset))"
    setFocusedInternal :: CString -> IO ()

setFocused :: String -> IO ()
setFocused s = withCString s setFocusedInternal

-- "id=simplified"

foreign import javascript "((arr,offset) => document.getElementById('simplified').value = h$decodeUtf8z(arr,offset))"
    setSimplifiedInternal :: CString -> IO ()

setSimplified :: String -> IO ()
setSimplified s = withCString s setSimplifiedInternal

-- "id=evaluation"

foreign import javascript "((arr,offset) => document.getElementById('evaluation').value = h$decodeUtf8z(arr,offset))"
    setEvaluationInternal :: CString -> IO ()

setEvaluation :: String -> IO ()
setEvaluation s = withCString s setEvaluationInternal
