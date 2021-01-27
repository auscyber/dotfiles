{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TupleSections #-}
module Stack () where

import           XMonad
import qualified XMonad.Util.ExtensibleState as XS 
import qualified XMonad.StackSet as W
import           Control.Lens


data WorkspaceSet wsId i l a = 
    WorkspaceSet { _currentWorkspace :: W.Workspace i l a
                 , _visibleWorkspaces :: [W.Workspace i l a]
                 , _hiddenWorkspaces :: [W.Workspace i l a]
                 , _workspaceId :: wsId
                 }
$(makeLenses ''WorkspaceSet)


data WorkspaceState wsId i l a = WorkspaceState { currentWorkspaceSet :: wsId, workspaceSets :: [WorkspaceSet wsId i l a] } deriving (Typeable, Read,Show)

instance ExtensionClass (WorkspaceState String i l a where
    initialValue = WorkspaceState { currentWorkspaceSet = "default", workspaceSets = mempty }
    extensionType = PersistentExtension a


replaceStackSet :: wsId 
                   -> WorkspaceSet wsId i l a
                   -> W.StackSet i l a sid sd
                   -> (W.StackSet i l a sid sd,WorkspaceSet wsId i l a)
replaceStackSet str wsset stack = 
      let (newCurrent,oldCurrent) = changeWorkspace (view currentWorkspace wsset) (W.current stack)
          (newVisible,oldVisible) = changeVisible (view visibleWorkspaces wsset) (W.visible stack)
          (newHiddenWorkspaces,oldHiddenWorkspaces) = changeHiddenWorkspaces (view hiddenWorkspaces wsset) (W.hidden stack)
          newStack = stack { W.current = newCurrent, W.visible = newVisible, W.hidden = newHiddenWorkspaces}
          newWorkspaceSet = WorkspaceSet { _currentWorkspace = oldCurrent
                                         , _visibleWorkspaces = oldVisible
                                         , _hiddenWorkspaces = oldHiddenWorkspaces
                                         , _workspaceId = str 
                                         } in (newStack,newWorkspaceSet)

changeHiddenWorkspaces :: [W.Workspace i l a] 
                          -> [W.Workspace i l a]
                          -> ([W.Workspace i l a],[W.Workspace i l a])
changeHiddenWorkspaces = tupleZip (,)
changeVisible :: [W.Workspace i l a] -> [W.Screen i l a sid sd] -> ([W.Screen i l a sid sd],[W.Workspace i l a])
changeVisible = tupleZip changeWorkspace

changeWorkspace :: W.Workspace i l a -> W.Screen i l a sid sd -> (W.Screen i l a sid sd,W.Workspace i l a )
changeWorkspace ws srcn@W.Screen{ W.workspace = workspace' } = (srcn {W.workspace = ws},ws)


tupleZip :: (a -> b -> (c,d)) -> [a] -> [b] -> ([c],[d])
tupleZip f a b = (a',b')
    where set = zipWith f a b
          a' = map fst set
          b' = map snd set


debug :: MonadIO m => String -> m ()
debug x = io $ writeFile x "~/.debug-log"
