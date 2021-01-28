{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TupleSections #-}
module WorkspaceSet (createWsKeybind,fixTag,changeWorkspaces,filterOutInvalidWSet,nextWorkspaceSet, previousWorkspaceSet, WorkspaceSetId,workspaceSetHook,runOnWorkspace,workspaceSetKeys) where
import System.IO
import           XMonad
import qualified XMonad.Util.ExtensibleState as XS 
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import           Control.Lens
import           Data.List (nubBy,find)
import           Data.Bifunctor
import           Data.Maybe (fromMaybe)
import           Control.Monad
import Control.Applicative
import qualified StackSetLens as SL
import           Data.Monoid
import qualified XMonad.Core as Core

type WorkspaceSetId = String

data WorkspaceSet  = 
    WorkspaceSet { _workspaceSetName :: WorkspaceSetId
                 , _workspaceNames :: [WorkspaceId]
                 } deriving (Typeable,Read,Show)

$(makeLenses ''WorkspaceSet)


data WorkspaceState = 
    WorkspaceState { _currentWorkspaceSet :: WorkspaceSet
                   , _workspaceSetsUp :: [WorkspaceSet] 
                   , _workspaceSetsDown :: [WorkspaceSet]
                   , _origInput :: [(WorkspaceSetId,[WorkspaceId])]
                   } deriving (Typeable,Read,Show)

$(makeLenses ''WorkspaceState)
--Discriminator
discrim = "-|-"

instance  ExtensionClass WorkspaceState where
    initialValue = WorkspaceState { _currentWorkspaceSet = WorkspaceSet "default" [], _workspaceSetsUp = mempty, _workspaceSetsDown = mempty , _origInput = mempty}
--    extensionType = PersistentExtension 

runOnWorkspace :: WorkspaceSetId -> X () -> X()
runOnWorkspace wsSet act = do
    currentWorkspaceSet <- XS.gets (view currentWorkspaceSet)
    when (_workspaceSetName currentWorkspaceSet == wsSet) act

newWorkspaceState :: [WorkspaceId] -> [WorkspaceSet] -> WorkspaceState
newWorkspaceState orig wsSets =WorkspaceState ( WorkspaceSet "default" orig) mempty wsSets mempty
getWorkspaces :: WorkspaceState -> [WorkspaceSet]
getWorkspaces = liftA3 (\x y z -> z:(x<>y)) _workspaceSetsUp _workspaceSetsDown _currentWorkspaceSet




workspaceSetHook :: [(WorkspaceSetId,[WorkspaceId])] -> ManageHook
workspaceSetHook wssets = do
    l <- liftX $ asks (layoutHook . config)
    orig' <- liftX $ XS.gets _origInput
    if orig' /= wssets 
    then do
            let wssets' = map (uncurry WorkspaceSet) wssets
            ws <- liftX $ asks (workspaces . config)
            liftX $ XS.put (newWorkspaceState ws wssets')
            liftX $ XS.modify (set origInput wssets)
            workspaceSetHook wssets
       else do
        modifiedTags <- liftX $ XS.gets (concatMap modifyTags  . getWorkspaces)
        
        pure (Endo $ f modifiedTags l)
   where 
    f modifiedTags layout winset = 
        let tags = map W.tag $ W.workspaces winset
            createWorkspace tag = W.Workspace tag layout Nothing
        in 
            foldr (\x acc -> if x`elem` tags then acc else over SL.hiddenLens (createWorkspace x:) acc) 
                winset modifiedTags

fixTag :: WorkspaceSetId -> WorkspaceId -> WorkspaceId
fixTag wsSet 
    | wsSet /= "default" = ((wsSet++discrim)++)
    | otherwise = id         

modifyTags :: WorkspaceSet -> [WorkspaceId]
modifyTags = liftA2 (map . fixTag ) (view workspaceSetName) (view workspaceNames) 

nextWorkspaceSet :: WorkspaceState -> WorkspaceState
nextWorkspaceSet ws
    | views workspaceSetsDown null ws = ws
    | otherwise = 
        let (newWorkspaceSet:workspaceSetsDown') = _workspaceSetsDown ws
            oldWorkspaceSet = _currentWorkspaceSet ws
        in over workspaceSetsUp (oldWorkspaceSet:)  . (workspaceSetsDown .~ workspaceSetsDown') . (currentWorkspaceSet .~ newWorkspaceSet) $ ws

previousWorkspaceSet :: WorkspaceState -> WorkspaceState
previousWorkspaceSet ws
    | views workspaceSetsUp null ws = ws
    | otherwise = 
        let (newWorkspaceSet:workspaceSetsUp') = _workspaceSetsUp ws
            oldWorkspaceSet = _currentWorkspaceSet ws
        in over workspaceSetsDown (oldWorkspaceSet:)  . (workspaceSetsUp .~ workspaceSetsUp') . (currentWorkspaceSet .~ newWorkspaceSet) $ ws

filterOutInvalidWSet :: PP -> X PP
filterOutInvalidWSet pp = do
    invalid <- ignoreWsSet
    right <- XS.gets (modifyTags . _currentWorkspaceSet)
    pure $ 
        pp { ppSort = fmap (. {-over (traverse . SL.tagLens) (cleanWS at)  . -}  f invalid) (ppSort pp)
           , ppCurrent = ppSection ppCurrent right
           , ppVisible = ppSection ppVisible right
           , ppHidden = ppSection ppHidden right
           , ppHiddenNoWindows = ppSection ppHiddenNoWindows  right
           }
  where 
    f invalid = filter (not . (`elem` invalid) . W.tag)
    ppSection f at =  f pp . cleanWS at 

createWsKeybind :: (KeyMask,KeySym) -> [(WorkspaceSetId ,X ())] -> ((KeyMask,KeySym),X ())
createWsKeybind key binds = (key,f)
  where 
    cleanWS = nubBy (\x y -> fst x == fst y) binds
    f = do
        origWS <- asks (workspaces . config)
        XS.gets (view (currentWorkspaceSet . workspaceSetName)) >>= \ws -> fromMaybe (return ()) (snd <$> find ((ws ==) . fst) cleanWS)




changeWorkspaces :: WorkspaceSetId -> [WorkspaceId] -> (WorkspaceSetId,[((KeyMask,KeySym ),X ())])
changeWorkspaces wsSet ws = (wsSet,[((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (map (fixTag wsSet ) ws) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]])


cleanWS :: [WorkspaceId] -> WorkspaceId -> WorkspaceId 
cleanWS at wsId =
    if wsId `elem` at then
        fromMaybe wsId (checkFunc wsId [])
    else wsId
  where 
    checkFunc :: String -> String -> Maybe String
    checkFunc (x:xs) acc 
        | take (length discrim) acc  == discrim =  Just (x:xs)
        | otherwise = checkFunc xs (x:acc)
    checkFunc [] _ = Nothing

ignoreWsSet :: X [WorkspaceId]
ignoreWsSet = do
    a <- XS.gets allTags
    debug (concat a)
    pure a

allTags :: WorkspaceState -> [WorkspaceId]
allTags = concatMap modifyTags . liftA2 (<>) (view workspaceSetsUp) (view workspaceSetsDown)


workspaceSetKeys :: WorkspaceSetId -> [(a,X ())] -> [(a, X ())]
workspaceSetKeys = map  . second . runOnWorkspace

debug :: MonadIO m => String -> m ()
debug x = io $ appendFile "/home/auscyber/.debug-log" (x<>"\n")

