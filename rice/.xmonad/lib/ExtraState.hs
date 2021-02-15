module ExtraState (ExtraState,dbus_client,workspaceNames) where

import qualified Data.Map as M
import           XMonad
import           DBus
import           DBus.Client
import           XMonad.Prompt
import           XMonad.Hooks.DynamicIcons
data ExtraState = ExtraState { dbus_client :: X Client, workspaceNames :: M.Map String Icon} deriving Typeable 

instance ExtensionClass ExtraState where
   initialValue = 
        ExtraState { 
            dbus_client = io connectSession 
            , workspaceNames = M.fromList $ zipWith (\x _ -> (show x,Icon  "\xf111" "\xf111""\xf10c""\xf10c")) [1..9] [1..] }

