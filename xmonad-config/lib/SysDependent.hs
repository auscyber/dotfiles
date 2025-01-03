{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StrictData #-}

module SysDependent where

import XMonad

import qualified Data.Map as M
import Data.Typeable (cast)

import qualified XMonad.Util.ExtensibleConf as XC

handleModeF :: [(String, ExtraConfig)] -> [String] -> XConfig Layout -> IO (XConfig Layout)
handleModeF confs ("--system" : system : xs) conf =
    pure $ maybe conf (`XC.add` conf) (lookup system confs)
handleModeF _ _ conf = pure conf

data ExtraConfig = ExtraConfig
    { titleLength :: !Int
    , launchApps :: ![String]
    , onceApps :: ![String]
    }
    deriving (Typeable)

instance Semigroup ExtraConfig where
    (<>) a b = b
