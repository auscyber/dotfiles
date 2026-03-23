{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ExtraState (ExtraState, dbus_client, workspaceNames) where

import Control.Applicative
import DBus.Client
import qualified Data.Map as M
import XMonad
import XMonad.Hooks.DynamicIcons

data ExtraState = ExtraState
    { dbus_client :: X Client
    , workspaceNames :: M.Map String String
    }
    deriving (Typeable)

instance ExtensionClass ExtraState where
    initialValue =
        ExtraState
            { dbus_client = io connectSession
            , workspaceNames = M.fromList $ (zip <*> id) (map show [1 .. 9])
            }

twoArguments :: (a -> a -> b) -> a -> b
twoArguments f = f <*> id
