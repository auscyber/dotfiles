{-# LANGUAGE OverloadedStrings #-}

module Config (myConfig, ExtraState) where

import Control.Monad ((>=>))
import DBus ()
import DBus.Client (Client)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Function (on)
import Data.Functor ()
import Data.List (
    elemIndex,
    isSuffixOf,
    nub,
    nubBy,
 )
import qualified Data.Map as M
import Data.Maybe
import Eww
import ExtraState (ExtraState (dbus_client))
import Media (next, playPause, previous)
import Polybar (polybarPP, switchMoveWindowsPolybar)
import WorkspaceSet (
    WorkspaceSetId,
    moveToNextWsSet,
    moveToPrevWsSet,
    nextWSSet,
    prevWSSet,
 )
import XMonad
import XMonad.Actions.Commands ()
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.KeyRemap (
    KeymapTable (..),
    emptyKeyRemap,
    setDefaultKeyRemap,
    setKeyRemap,
 )
import XMonad.Actions.Search (isPrefixOf)
import XMonad.Actions.WorkspaceNames (
    renameWorkspace,
    workspaceNamesPP,
 )
import XMonad.Hooks.DynamicIcons (
    IconConfig (iconConfigFilter, iconConfigFmt, iconConfigIcons),
    appIcon,
    dynamicIconsPP,
    iconsFmtReplace,
    iconsGetAll,
    iconsGetFocus,
    wrapUnwords,
 )
import XMonad.Hooks.DynamicLog (
    PP (PP, ppOutput, ppRename),
    dynamicLogString,
    filterOutWsPP,
    pad,
    trim,
 )
import XMonad.Hooks.EwmhDesktops (
    addEwmhWorkspaceSort,
    ewmh,
    ewmhDesktopsEventHook,
    ewmhDesktopsLogHookCustom,
    ewmhDesktopsStartup,
    ewmhFullscreen,
    fullscreenEventHook,
 )
import XMonad.Hooks.ManageDocks (
    avoidStruts,
    docks,
    manageDocks,
 )
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.ScreenCorners ()
import XMonad.Hooks.ServerMode (serverModeEventHookCmd)
import XMonad.Hooks.WindowSwallowing
import qualified XMonad.Util.ExtensibleConf as XC

-- import XMonad.Hooks.WindowSwallowing ()
import qualified XMonad.Layout.Fullscreen as F

---import XMonad.Layout.LayoutModifier ()

import qualified Laptop
import qualified Laptop as Desktop
import SysDependent (ExtraConfig (launchApps, onceApps), handleModeF)
import Text.Read (readMaybe)
import XMonad.Layout.MultiToggle ()
import XMonad.Layout.MultiToggle.Instances ()
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.SimpleFloat (simpleFloat)
import XMonad.Layout.Spacing (Border (Border), spacingRaw)
import XMonad.Layout.Tabbed
import XMonad.Prompt (
    XPConfig (fgColor, font, position),
    XPPosition (CenteredAt),
 )
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window ()
import XMonad.Prompt.XMonad ()
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor ()
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Hacks (
    javaHack,
    windowedFullscreenFixEventHook,
 )
import XMonad.Util.NamedScratchpad (
    NamedScratchpad (NS, name),
    NamedScratchpads,
    defaultFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    nonFloating,
    scratchpadWorkspaceTag,
 )
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare (filterOutWs)

extraConfigs =
    [ ("laptop", Laptop.dependentConf)
    , ("desktop", Desktop.dependentConf)
    ]

onLaunch =
    [ "picom --experimental-backends --daemon --dbus --config ~/.config/picom/picom2.conf"
    , "lxpolkit"
    , "dunst"
    , "~/.config/polybar/launch.sh"
    , "~/.screenlayout/layout.sh"
    , "nitrogen --restore "
    , "xset m 0 0"
    , "xset s on && xset s 300"
    , "1password --silent"
    , "xss-lock i3lock"
    , "nm-applet"
    , "blueman-applet"
    , "skype"
    , "mpris-proxy"
    ]

gameMap :: KeymapTable
gameMap =
    KeymapTable
        [ ((0, xK_1), (0, xK_Left))
        , ((0, xK_2), (0, xK_Up))
        , ((0, xK_minus), (0, xK_Down))
        , ((0, xK_plus), (0, xK_Right))
        ]

mountRclone :: (String, String, [String]) -> String
mountRclone (name, location, extra_args) = concat ["rclone mount ", name, ": ", location, " ", unwords extra_args, " --daemon"]

once = []

myStartupHook = do
    --    spawn
    --        "op signin"
    extra <- fmap SysDependent.launchApps <$> XC.ask
    mapM_ (\x -> spawn (x ++ " &")) $ onLaunch ++ fromMaybe [] extra
    extraOnce <- fmap SysDependent.onceApps <$> XC.ask
    mapM_
        (\x -> spawnOnce (x ++ " &"))
        $ once ++ fromMaybe [] extraOnce

    --  addScreenCorner SCLowerRight (spawn "alacritty")
    setDefaultKeyRemap emptyKeyRemap [gameMap, emptyKeyRemap]
    io $ mapM_ (safeSpawn "mkfifo" . (: [])) ["/tmp/.xmonad-workspace-log", "/tmp/xmonad-status-json.log"]

--     setDefaultCursor xC_left_ptr

dbusAction :: (Client -> IO ()) -> X ()
dbusAction action = XS.gets dbus_client >>= (id >=> io . action)

getWorkspaceText :: M.Map Int String -> String -> String
getWorkspaceText xs n = fromMaybe n $ readMaybe n >>= flip M.lookup xs

myWorkspaces = map show [1 .. 9]

removedKeys :: [String]
removedKeys = ["M-p", "M-S-p", "M-S-e", "M-S-o", "M-b"]

myConfig =
    javaHack
        .
        --    flip additionalKeys (createDefaultWorkspaceKeybinds myConfig workspaceSets) $
        flip additionalKeysP myKeys
        . flip removeKeysP removedKeys
        . ewmhFullscreen
        . docks
        . ewmh
        . addEwmhWorkspaceSort (pure $ filterOutWs [scratchpadWorkspaceTag])
        $ def
            { terminal = myTerm
            , borderWidth = myBorderWidth
            , normalBorderColor = secondaryColor
            , focusedBorderColor = mainColor
            , workspaces = myWorkspaces
            , modMask = mod4Mask
            , focusFollowsMouse = False
            , startupHook = myStartupHook
            , --      , borderWidth = 1
              --    , logHook = dynamicLogWithPP (polybarPP workspaceSymbols )
              logHook = polybarLogHook >> ewwLogHook {- do
                                                     wsNames <- XS.gets workspaceNames
                                                     workspaceFilter (iconConfig $ polybarPP wsNames)  >>= \x -> cleanWS' >>= \y ->  ewmhDesktopsLogHookCustom (y ) -}
            , manageHook = myManageHook
            , layoutHook = myLayout
            , handleEventHook = myEventHook
            , handleExtraArgs = handleModeF extraConfigs
            }

myEventHook =
    mconcat
        [ serverModeEventHookCmd
        , handleEventHook def
        , windowedFullscreenFixEventHook
        , swallowEventHook (className =? "org.wezfurlong.wezterm") (not <$> (className =? "org.wezfurlong.wezterm"))
        --      screenCornerEventHook
        ]

myIconConfig :: IconConfig
myIconConfig =
    def
        { iconConfigFmt = iconsFmtReplace (\xs -> if length xs > 1 then wrapUnwords "[" "]" . map trim $ xs else mconcat xs)
        , iconConfigIcons = icons
        , iconConfigFilter = iconsGetAll
        }

polybarLogHook :: X ()
polybarLogHook = do
    str <- gets (map W.tag . W.workspaces . windowset)
    pp <-
        polybarPP myWorkspaces
            >>= dynamicIconsPP myIconConfig
            >>= fmap (switchMoveWindowsPolybar str . filterOutWsPP [scratchpadWorkspaceTag]) <$> workspaceNamesPP
    io . ppOutput pp =<< dynamicLogString pp

-- >> writeCurrentState pp "/tmp/xmonad-status-json.log"

ewwLogHook :: X ()
ewwLogHook = do
    str <- gets (map W.tag . W.workspaces . windowset)
    pp <-
        dynamicIconsPP myIconConfig (ewwPP myWorkspaces)
            >>= fmap (filterOutWsPP [scratchpadWorkspaceTag]) <$> workspaceNamesPP
    io . ppOutput pp =<< dynamicLogString pp

--                        >> writeCurrentState pp "/tmp/xmonad-status-json.log"

workspaceSets :: [(WorkspaceSetId, [WorkspaceId])]
workspaceSets =
    [ ("bob", map show [1 .. 5])
    , ("jim", map show [1 .. 4])
    ]

browserQuery = className =? "google-chrome"

icons :: XMonad.Query [String]
icons =
    foldMap
        (fmap (fmap pad))
        [ className =? "discord" --> appIcon "\xfb6e"
        , className =? "google-chrome" --> appIcon "\xf268"
        , browserQuery --> appIcon "\63288"
        , className =? "Spotify" <||> className =? "spotify" --> appIcon "阮"
        , className =? "jetbrains-idea" --> appIcon "\xe7b5"
        , className =? "Skype" --> appIcon "\61822"
        , (("vim" `isPrefixOf`) <$> title <&&> (className =? "org.wezfurlong.wezterm")) --> appIcon "\59333"
        ]

theme :: Theme
theme =
    def
        { activeColor = mainColor
        , inactiveColor = secondaryColor
        , activeTextColor = "#121212"
        --        , fontName = "Hasklug Nerd Font"
        }

-- Colors
mainColor = "#FFEBEF"

--
secondaryColor = "#8BB2C1"

-- mainColor = "#FFDB9E"
-- secondaryColor = "#ffd1dc"

tertiaryColor = "#A3FFE6"

myTerm = "wezterm"

myBorderWidth = 1

myLayout =
    tiled ||| tab ||| smartBorders Full ||| avoidStruts simpleFloat
  where
    tab = rename "Tabbed" . avoidStruts . gaps $ tabbed shrinkText theme
    tiled = rename "Tiled" . conf $ Tall nmaster delta ratio

    conf = avoidStruts . gaps . smartBorders
    gaps = spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True

    rename = renamed . ((: []) . Replace)

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion
    -- Move focus to the next window
    ratio = 1 / 2

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

myManageHook =
    composeAll
        [ className =? "Gimp" --> doFloat
        , -- className =? "Firefox" --> doShift (myWorkspaces !! 1),),
          className =? "Steam" --> doFloat
        , className =? "steam" --> doFloat
        , className =? "jetbrains-idea" --> doFloat
        , className =? "Spotify" --> doShift "4"
        , className =? "Ghidra" --> doFloat
        , className =? "zoom" --> doFloat
        , ("Minecraft" `isPrefixOf`) <$> className --> doFullFloat
        , namedScratchpadManageHook scratchpads
        , manageDocks
        , className =? "ableton live 10 lite.exe" --> doFloat
        --    , className =? "Xmessage" --> doFloat
        --    , workspaceSetHook workspaceSets
        ]

type NamedScratchPadSet = [(String, NamedScratchpad)]

scratchpadSet :: NamedScratchPadSet
scratchpadSet =
    [ ("M-C-s", NS "spotify" "spotify" (className =? "Spotify") defaultFloating)
    , ("M-C-o", NS "obsidian" "obsidian" (className =? "obsidian") nonFloating)
    , ("M-C-d", NS "discord" "discord" (("discord" `isSuffixOf`) <$> className) nonFloating)
    , ("M-S-C-m", NS "skype" "skypeforlinux" (className =? "Skype") defaultFloating)
    , ("M-C-t", NS "terminal" (myTerm ++ " -t \"scratchpad term\"") (title =? "scratchpad term") defaultFloating)
    , ("M-C-m", NS "mail" "thunderbird" (className =? "Mail" <||> className =? "Thunderbird") nonFloating)
    , ("M-S-C-t", NS "teams" "teams" (className =? "Microsoft Teams - Preview") nonFloating)
    , ("M-C-a", NS "authy" "authy" (className =? "Authy Desktop") defaultFloating)
    , ("M-C-p", NS "1password" "1password" (className =? "1Password") nonFloating)
    ]

getScratchPads :: NamedScratchPadSet -> NamedScratchpads
getScratchPads = map snd

getScratchPadKeys :: NamedScratchPadSet -> [(String, X (), Maybe String)]
getScratchPadKeys ns = map mapFunc ns
  where
    mapFunc (key, NS{name = name'}) = (key, namedScratchpadAction ns' name', doc $ "Toggle Scratchpad " ++ name')
    ns' = getScratchPads ns

scratchpads = getScratchPads scratchpadSet

scratchpadKeys = getScratchPadKeys scratchpadSet

appKeys =
    map
        (first spawn)
        [ ("M-S-r", "~/.config/rofi/bin/launcher_colorful", doc "Launch rofi")
        , -- Start alacritty
          ("M-S-t", myTerm, doc $ "Launch " ++ myTerm)
        , -- Take screenshot
          ("M-S-s", "~/.xmonad/screenshot-sec.sh", doc "Take screenshot")
        , -- Chrome
          ("M-S-g", "google-chrome-stable", doc "Launch Chrome")
        , -- Start emacs
          ("M-d", "emacsclient -c", doc "Start emacs client")
        , -- start nvim
          ("M-S-n", "wezterm start -- nvim", doc "Launch neovim with neovim")
        ]

myKeys = map (\(x, y, _) -> (x, y)) keyCombination
documentation = concatMap (\(x, _, y) -> maybeToList y >>= \a -> x ++ " " ++ a ++ "\n") keyCombination
keyCombination =
    concat
        [ scratchpadKeys
        , multiScreenKeys
        , appKeys
        , customKeys
        --            , workspaceKeys
        ]

multiScreenKeys =
    [ ("M" ++ m ++ key, screenWorkspace sc >>= flip whenJust (windows . f), doc (action ++ "Display #" ++ show sc))
    | (key, sc) <- zip ["-e", "-w"] [0 ..]
    , (f, m, action) <- [(W.view, "", "View "), (W.shift, "-S", "Shift focused window to")]
    ]

noDoc = Nothing
doc = Just

customKeys =
    [ ("<XF86AudioPrev>", dbusAction previous, Just "Previous track")
    , ("<XF86AudioNext>", dbusAction next, Just "Next Track")
    , ("<XF86AudioPlay>", dbusAction playPause, Just "Play")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%", Just "Raise Volume")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%", Just "Lower Volume")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle", Just "Mute Audio")
    , --  Reset the layouts on the current workspace to default
      -- Swap the focused and the master window
      -- Polybar toggle
      ("M-b", spawn "polybar-msg cmd toggle", Just "Toggle Polybar")
    , --    , ("M-r", promptSearchBrowser promptConfig "chromium" hoogle )
      ("M-r", renameWorkspace promptConfig, Just "Rename Current Workspace")
    , ("M-C-r", shellPrompt promptConfig, Just "Open xmonad run prompt")
    , ("M-m", nextWSSet True, Nothing)
    , ("M-n", prevWSSet True, Nothing)
    , --    , ("M-S-m", moveToNextWsSet True, Nothing)
      --    , ("M-S-n", moveToPrevWsSet True, Nothing)
      ("M-C-`", setKeyRemap gameMap, Nothing)
    , ("M-C-S-`", setKeyRemap emptyKeyRemap, Nothing)
    , ("M-C-h", spawn $ "xmessage \'" ++ documentation ++ "\'", doc "Show help")
    --
    --    , ("M1-<Tab>", nextWS )
    --    , ("M1-S-<Tab>", prevWS)S)
    ]

promptConfig =
    def
        { fgColor = mainColor
        , position = CenteredAt 0.3 0.5
        , font = "xft:Hasklug Nerd Font:style=Regular:size=12"
        }

workspaceKeys =
    [ ("M-L", nextWS)
    , ("M-h", prevWS)
    ]
