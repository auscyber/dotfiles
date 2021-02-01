{-# LANGUAGE RankNTypes, TemplateHaskell, DeriveDataTypeable, TupleSections #-}
module WorkspaceSet (
        cleanWS',createDefaultWorkspaceKeybinds,

        switchToWsSet,createWsKeybind,fixTag,changeWorkspaces,filterOutInvalidWSet,nextWSSet, prevWSSet, WorkspaceSetId,workspaceSetHook) where
import System.IO
import           XMonad
import qualified XMonad.Util.ExtensibleState as XS 
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import           Control.Lens
import           Data.List (nub,nubBy,find)
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
                 , _currentWorkspaceTag :: WorkspaceId
                 } deriving (Typeable,Read,Show)

$(makeLenses ''WorkspaceSet)


data WorkspaceState = 
    WorkspaceState { _currentWorkspaceSet :: WorkspaceSet
                   , _workspaceSetsUp :: [WorkspaceSet]
                   , _workspaceSetsDown :: [WorkspaceSet]
                   , _initialised :: Bool
                   } deriving (Typeable,Read,Show)

$(makeLenses ''WorkspaceState)



--Discriminator
discrim = "-|-"

instance  ExtensionClass WorkspaceState where
    initialValue = WorkspaceState (WorkspaceSet "default" [] mempty) mempty  mempty False
    extensionType = PersistentExtension 

runOnWorkspace :: WorkspaceSetId -> X () -> X()
runOnWorkspace wsSet act = do
    currentWorkspaceSet <- XS.gets (view currentWorkspaceSet)
    when (_workspaceSetName currentWorkspaceSet == wsSet) act

newWorkspaceState :: [WorkspaceId] -> [WorkspaceSet] -> WorkspaceState
newWorkspaceState orig wsSets =WorkspaceState (createWorkspaceSet "default" orig ) mempty wsSets False


getWorkspaceSets :: WorkspaceState -> [WorkspaceSet]
getWorkspaceSets = (<>) <$> view workspaceSetsUp <*> view workspaceSetsDown

getWorkspacesWithCurrent :: WorkspaceState -> [WorkspaceSet]
getWorkspacesWithCurrent = (<>) 
    <$> (reverse . view workspaceSetsUp) 
    <*> ((:) <$> view currentWorkspaceSet <*> view workspaceSetsDown)

createWorkspaceSet :: WorkspaceSetId -> [WorkspaceId] -> WorkspaceSet 
createWorkspaceSet wsId (x:xs) = WorkspaceSet wsId (x:xs) x

workspaceSetHook :: [(WorkspaceSetId,[WorkspaceId])] -> ManageHook
workspaceSetHook wssets = do
    l <- liftX $ asks (layoutHook . config)
    initialised' <- liftX $ XS.gets _initialised
    if not initialised'
    then do
            let wssets' = map (uncurry createWorkspaceSet) wssets
            ws <- liftX $ asks (workspaces . config)
            liftX $ XS.put (newWorkspaceState ws wssets')
            liftX $ XS.modify (set initialised True)
            workspaceSetHook wssets
       else do
        modifiedTags <- liftX $ XS.gets (concatMap modifyTags  . getWorkspaceSets)
        tag' <- liftX $ gets (W.currentTag . windowset) 
        liftX $ XS.modify (set (currentWorkspaceSet . currentWorkspaceTag) .  flip cleanWS tag' . modifyTags <$> view currentWorkspaceSet  <*> id)
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




switchToWsSet :: WorkspaceSetId -> X ()
switchToWsSet wsSetId = do
    runQuery (workspaceSetHook mempty) 0
    workspaceSets <- XS.gets getWorkspacesWithCurrent
    case break ((==wsSetId) . view workspaceSetName) workspaceSets of
        (xs,ws:ss) -> do 
            XS.modify (set currentWorkspaceSet ws)
            io $ appendFile "/home/auscyber/invalidws.log" ("invalid WS" <> wsSetId <> "\n")
            windows $ W.greedyView (fixTag (_workspaceSetName ws) (_currentWorkspaceTag ws))
            XS.modify (set workspaceSetsDown ss)
            XS.modify (set workspaceSetsUp (reverse xs))
            join (asks (logHook . config))
        _ ->  io $ appendFile "/home/auscyber/.debug-log" "failed" 
  where 
    f :: (ScreenId,Core.WindowSpace) -> W.Screen WorkspaceId (Layout Window) Window ScreenId sd  -> (W.Screen WorkspaceId (Layout Window) Window ScreenId sd,Maybe Core.WindowSpace)
    f 
      (scrnId,workspace') 
      scrn@(W.Screen workspace'' scrnId' _)
        | scrnId == scrnId' = (scrn { W.workspace = workspace'},Just workspace'')
        | otherwise = (scrn,Nothing)



filterOutInvalidWSet :: PP -> X PP
filterOutInvalidWSet pp = do
    invalid <- ignoreWsSet
    right <- XS.gets (modifyTags . _currentWorkspaceSet)
    name <- XS.gets (view (currentWorkspaceSet . workspaceSetName))
    pure $ 
        pp { ppSort = fmap (. {-over (traverse . SL.tagLens) (cleanWS right)  .-}   f invalid) (ppSort pp)
           , ppCurrent = ppSection ppCurrent right
           , ppVisible = ppSection ppVisible right
           , ppHidden = ppSection ppHidden right
           , ppHiddenNoWindows = ppSection ppHiddenNoWindows  right
           , ppOrder = \(x:xs) -> if name == "default" then x:xs else x:name:xs
           }
  where 
    f invalid = filter (not . (`elem` invalid) . W.tag)
    ppSection f at =  f pp . cleanWS at 

createWsKeybind :: Eq a => a -> [(WorkspaceSetId ,X ())] -> (a,X ())
createWsKeybind key binds = (key,f)
  where 
    cleanWS = nubBy (\x y -> fst x == fst y) binds
    f = do
        origWS <- asks (workspaces . config)
        XS.gets (view (currentWorkspaceSet . workspaceSetName)) >>= \ws -> maybe (return ()) snd (find ((ws ==) . fst) cleanWS)

cleanWS' :: X ([WindowSpace]  -> [WindowSpace])
cleanWS' = do
    invalid <- ignoreWsSet
    right <- XS.gets (modifyTags . _currentWorkspaceSet)
    pure $ over (traverse . SL.tagLens) (cleanWS  right)
    where f invalid = filter (not . (`elem` invalid) . W.tag)

changeWorkspaces :: WorkspaceSetId -> [WorkspaceId] -> (WorkspaceSetId,[((KeyMask,KeySym ),X ())])
changeWorkspaces wsSet ws = (wsSet,[((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (map (fixTag wsSet ) ws) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]])

createDefaultWorkspaceKeybinds :: XConfig l -> [(WorkspaceSetId,[WorkspaceId])] -> [((KeyMask,KeySym),X ())]
createDefaultWorkspaceKeybinds xconf = createKeybinds' . map (uncurry changeWorkspaces) . (("default",workspaces xconf):)

createKeybinds' :: Eq a => [(WorkspaceSetId,[(a,X ())])] -> [(a,X ())]
createKeybinds' keys = map f keySets
    where keySets = nub $ concatMap (map fst . snd) keys
          f key = createWsKeybind key $ [(wsId, action) | (wsId,actions) <- keys,(key',action) <- actions,key' == key] 

nextWSSet :: Bool  -- ^ Cycle workspacsets
             -> X ()
nextWSSet cycle = do
    a <- XS.gets ( view workspaceSetsDown)
    case a of
        [] -> when cycle $ do
           a <- XS.gets (reverse . view workspaceSetsUp) 
           case a of
                (x:xs) -> switchToWsSet (_workspaceSetName x)
                _ -> pure ()
        (x:_) -> switchToWsSet (_workspaceSetName x)

prevWSSet :: Bool
             -> X ()
prevWSSet cycle = do
    a <- XS.gets (  view workspaceSetsUp)
    case a of
        [] -> when cycle $ do
           a <- XS.gets (reverse . view workspaceSetsDown) 
           case a of
                (x:xs) -> switchToWsSet (_workspaceSetName x)
                _ -> pure ()
        (x:_) -> switchToWsSet (_workspaceSetName x)
   
cleanWS :: [WorkspaceId] -> WorkspaceId -> WorkspaceId 
cleanWS at wsId =
    if wsId `elem` at then
        fromMaybe (head at) (checkFunc wsId [])
    else wsId
  where 
    checkFunc :: String -> String -> Maybe String
    checkFunc (x:xs) acc 
        | take (length discrim) acc  == discrim =  Just (x:xs)
        | otherwise = checkFunc xs (x:acc)
    checkFunc [] _ = Nothing

ignoreWsSet :: X [WorkspaceId]
ignoreWsSet = XS.gets allTags

allTags :: WorkspaceState -> [WorkspaceId]
allTags = concatMap modifyTags . liftA2 (<>) (view workspaceSetsUp) (view workspaceSetsDown)

