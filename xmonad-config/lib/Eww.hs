{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Eww (writeCurrentState, FocusStatus (..), Workspace (..), OutputState (..), ewwPP) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.List
import Data.Maybe
import GHC.Generics
import Polybar
import XMonad
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as S
import XMonad.Util.NamedWindows

data FocusStatus = FocusVisibleNoWindows | FocusVisible | FocusFocused | FocusHidden | FocusHiddenNoWindows deriving (Generic)

instance ToJSON FocusStatus
instance FromJSON FocusStatus

data Workspace = Workspace
    { workspaceName :: String
    , state :: FocusStatus
    }
    deriving (Generic)

instance ToJSON Workspace
instance FromJSON Workspace

data OutputState = OutputState
    { stateWorkspaces :: [Workspace]
    , stateTitle :: String
    , layout :: String
    }
    deriving (Generic)

instance ToJSON OutputState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON OutputState

currentState :: PP -> X OutputState
currentState pp = do
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp

    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset

    -- window title
    wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (`catchX` return Nothing) $ ppExtras pp
    pure $
        OutputState
            { layout = ld
            , stateTitle = wt
            , stateWorkspaces = pprWorkspace sort' urgents pp winset
            }

writeCurrentState :: PP -> String -> X ()
writeCurrentState pp path = do
    st <- currentState pp
    io $ do
        appendFile path $ displayState st pp
        appendFile path "\n"

sexpression :: String -> [(String, String)] -> String -> String
sexpression name attributes da = "(" ++ name ++ " " ++ unwords (map (\(a, b) -> ":" ++ a ++ " " ++ b) attributes) ++ " " ++ da ++ ")"

box = sexpression "box"
button = sexpression "button"
label = sexpression "label"

displayState :: OutputState -> PP -> String
displayState OutputState{..} pp =
    box
        [ ("class", show "workspaces")
        , ("orientation", show "h")
        , ("space-evenly", "true")
        , ("halign", show "start")
        , ("spacing", show 10)
        ]
        $ unwords (snd $ mapAccumL (\x y -> (x + 1, displayWorkspce pp x y)) 0 stateWorkspaces)

displayWorkspce :: PP -> Int -> Workspace -> String
displayWorkspce pp index Workspace{..} = button [("onclick", show $ "wmctrl -s" ++ show index), ("class", show $ clss state)] $ "\"" ++ workspaceName ++ "\""
  where
    clss FocusVisibleNoWindows = "visible_no_windows"
    clss FocusVisible = "visible"
    clss FocusHidden = "hidden"
    clss FocusHiddenNoWindows = "hidden_no_windows"
    clss FocusFocused = "focused"

pprWorkspace :: ([WindowSpace] -> [WindowSpace]) -> [Window] -> PP -> WindowSet -> [Workspace]
pprWorkspace sort' urgents pp s =
    map (\x -> let (f, status) = func x in Workspace (f pp $ (ppRename pp =<< S.tag) x) status) . sort' $
        map S.workspace (S.current s : S.visible s) ++ S.hidden s
  where
    this = S.currentTag s
    visibles = map (S.tag . S.workspace) (S.visible s)
    func w
        | S.tag w == this = (ppCurrent, FocusFocused)
        | S.tag w `elem` visibles && isJust (S.stack w) = (ppVisible, FocusVisible)
        | S.tag w `elem` visibles = (ppVisible, FocusVisibleNoWindows)
        | isJust (S.stack w) = (ppHidden, FocusHidden)
        | otherwise = (ppHiddenNoWindows, FocusHiddenNoWindows)

wsSetBox :: FocusStatus -> Int -> String -> String
wsSetBox state index workspaceName = button [("onclick", show $ "wmctrl -s " ++ show index), ("class", show $ clss state)] $ "\"" ++ workspaceName ++ "\""
  where
    clss FocusVisibleNoWindows = "visible_no_windows"
    clss FocusVisible = "visible"
    clss FocusHidden = "hidden"
    clss FocusHiddenNoWindows = "hidden_no_windows"
    clss FocusFocused = "focused"

wrapQ :: String -> String
wrapQ = wrap "\"" "\""

ewwPP :: [String] -> PP
ewwPP defaultIcons =
    let iconCurrent x
            | x `elem` defaultIcons = "\xf111"
            | otherwise = x

        iconHidden x
            | x `elem` defaultIcons = "\xf10c"
            | otherwise = x
     in def
            { {-ppCurrent = polybarWorkspace (polybarUnderlineWithColor "#FFCFD1" . polybarColour 'F' "#FFDB9E" ) ws False
              , ppHidden = polybarWorkspace (polybarColour 'F' "#E88B84") ws True
              , ppVisible = polybarWorkspace (polybarColour 'F' "#FFC9AB"  . wrap "[" "]") ws True
              , ppHiddenNoWindows = polybarWorkspace (polybarColour 'F' "#5754B3") ws True -}
              ppCurrent = wsSetBox FocusFocused 0 . pad . iconCurrent
            , ppHidden = wsSetBox FocusHidden 0 . pad . iconHidden
            , ppVisible = wsSetBox FocusVisible 0 . pad . iconCurrent
            , ppHiddenNoWindows = wsSetBox FocusHidden 0 . iconHidden
            , ppTitleSanitize = shorten 70 . ppTitle def
            , ppTitle = flip label [] . (: [("class", show "title")]) . ("text",) . wrapQ
            , ppWsSep = " "
            , ppLayout = label [] . wrapQ
            , ppOrder = \(x : xs) ->
                box
                    [ ("class", show "workspaces")
                    , ("orientation", show "h")
                    , ("space-evenly", "true")
                    , ("halign", show "start")
                    , ("spacing", show 10)
                    ]
                    x
                    : xs
            , ppSep = " "
            , -- , ppSep = polybarColour 'F' "#4D3636" " | "
              --             ppLayout = xmonadPolybarAction 1 "next-layout" . xmonadPolybarAction 3 "default-layout"
              --
              --  , ppOrder = \(x:xs) -> wrap "%{T2}" "%{T-}" x:xs
              --    , ppOrder = \(x:_:y) -> x:y
              ppOutput =
                appendFile "/tmp/xmonad-status-json.log"
                    . (++ "\n")
                    . box
                        [ ("spacing", "1")
                        , ("orientation", wrapQ "h")
                        , ("space-evenly", "false")
                        , ("halign", "center")
                        ]
            }
