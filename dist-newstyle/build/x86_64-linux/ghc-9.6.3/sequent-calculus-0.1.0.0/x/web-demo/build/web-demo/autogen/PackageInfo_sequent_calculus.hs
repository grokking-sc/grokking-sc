{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_sequent_calculus (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "sequent_calculus"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Compiling to the sequent calculus"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
