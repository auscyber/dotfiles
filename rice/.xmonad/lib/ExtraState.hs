module ExtraState (NameSet(..),WorkSpaceIconSet,ExtraState,dbus_client,workspaceNames) where

import qualified Data.Map as M
import XMonad
import           DBus
import           DBus.Client
import XMonad.Prompt

data NameSet l = 
        NameSet { current :: l
        , visible
        , hidden :: l,showOverlayIcons :: Bool }

type WorkSpaceIconSet = NameSet String
data ExtraState = ExtraState { dbus_client :: X Client, workspaceNames :: M.Map String (NameSet String)} deriving Typeable 

instance ExtensionClass ExtraState where
   initialValue = 
        ExtraState { 
            dbus_client = io connectSession 
            , workspaceNames = M.fromList $ zipWith (\x _ -> (show x,NameSet {showOverlayIcons = True, current = "\xf111",visible = "\xf111", hidden = "\xf10c"})) [1..9] [1..] }


