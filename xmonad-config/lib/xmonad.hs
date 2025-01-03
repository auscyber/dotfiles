{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Config (myConfig)
import Control.Monad (unless)
import qualified Laptop (dependentConf)
import System.Environment (getArgs, withArgs)
import System.Exit (exitFailure)
import qualified XMonad.Util.ExtensibleConf as XC

import XMonad

conf = myConfig

main = xmonad myConfig
