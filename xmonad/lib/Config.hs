{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Config (myConfig,ExtraState) where

import           Control.Monad
import           DBus
import           DBus.Client
import           Data.Bifunctor
import           Data.Function                       (on)
import           Data.Functor
import           Data.List                           (elemIndex, isSuffixOf,
                                                      nub, nubBy)
import qualified Data.Map                            as M
import           DynamicLog
import           ExtraState
import           Media
import           Polybar
import           System.Exit
import           WorkspaceSet
import           XMonad
import           XMonad.Actions.Commands
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Search
import           XMonad.Actions.WorkspaceNames
import           XMonad.Hooks.DynamicIcons
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.WindowSwallowing
import qualified XMonad.Layout.Fullscreen            as F
import           XMonad.Layout.Gaps
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace          (onWorkspace)
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                     as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState         as XS
import           XMonad.Util.Hacks
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows            (getName)
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce
import           SysDependent

onLaunch =
    [ "picom --experimental-backends --daemon --dbus"
    , "lxpolkit"
    , "dunst"
    , "~/.config/polybar/launch.sh"
    , "xrandr --output DP-0 --off --output DP-1 --off --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --mode 1920x1080 --pos 0x0 --rotate normal --output USB-C-0 --off"
    , "feh --bg-fill ~/backgrounds/ghost.png"
    , "xset m 0 0"
    ]


rclonemounts =
    [ ("driveschool","~/school_drive",["--vfs-cache-mode full","--vfs-cache-max-age 10m"])
    , ("drivepersonal","~/drive",[])
    , ("photos","~/Pictures",[])
    , ("onedrive_school","~/onedrive_school",[])
    ]

mountRclone :: (String,String,[String]) -> String
mountRclone (name,location,extra_args) = "rclone mount " ++ name ++ ": " ++ location ++ " " ++ unwords extra_args ++ " --daemon"

once = map mountRclone rclonemounts ++ ["emacs --daemon"]

myStartupHook = do
    ewmhDesktopsStartup
    mapM_ (\x -> spawn (x++" &")) onLaunch
    mapM_ (\x -> spawnOnce (x++" &")) once
    io $ forM_ [".xmonad-workspace-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
--     setDefaultCursor xC_left_ptr


dbusAction :: (Client -> IO ()) -> X ()
dbusAction action =  XS.gets dbus_client >>= (id >=> io . action)

getWorkspaceText :: M.Map Int String -> String -> String
getWorkspaceText xs n =
    case M.lookup (read n) xs of
        Just x -> x
        _      -> n

myWorkspaces = map show [1..9]


removedKeys :: [String]
removedKeys = ["M-p","M-S-p" , "M-S-e","M-S-o","M-b" ]
myConfig =
     javaHack $
     flip additionalKeys xineramaKeys $
--    flip additionalKeys (createDefaultWorkspaceKeybinds myConfig workspaceSets) $
    flip additionalKeysP myKeys $
    flip removeKeysP removedKeys $
    ewmhFullscreen $
    def {
       terminal = myTerm
      , borderWidth = myBorderWidth
      , normalBorderColor = secondaryColor
      , focusedBorderColor = mainColor
      , workspaces = myWorkspaces
      , modMask = mod4Mask
      , focusFollowsMouse = False
      , startupHook = myStartupHook
--      , borderWidth = 1
--    , logHook = dynamicLogWithPP (polybarPP workspaceSymbols )
      , logHook = myLogHook  {- do
        wsNames <- XS.gets workspaceNames
        workspaceFilter (iconConfig $ polybarPP wsNames)  >>= \x -> cleanWS' >>= \y ->  ewmhDesktopsLogHookCustom (y ) -}
      , manageHook = myManageHook
      , layoutHook =  myLayout
      , handleEventHook =  myEventHook
      }

myEventHook = mconcat
    [ ewmhDesktopsEventHook
    , fullscreenEventHook
    , serverModeEventHookCmd
    , handleEventHook def
    , docksEventHook
    , windowedFullscreenFixEventHook
    , swallowEventHook (className =? "Alacritty") (return True)
    ]



myLogHook :: X ()
myLogHook =
     XS.gets (polybarPP  . workspaceNames)
--    pure (polybarPP M.empty)
--        >>= workspaceNamesPP
    --  >>= DynamicLog.dynamicLogString . switchMoveWindowsPolybar . namedScratchpadFilterOutWorkspacePP>>=  io . ppOutput pp
    {- filterOutInvalidWSet pp -}
        >>= \pp -> dynamicIconsPP (def { iconConfigIcons = icons }) pp
        >>= DynamicLog.dynamicLogString . switchMoveWindowsPolybar  . filterOutWsPP [scratchpadWorkspaceTag]
        >>= io . ppOutput pp

workspaceSets :: [(WorkspaceSetId,[WorkspaceId])]
workspaceSets =
    [ ("bob",map show [1..5])
    , ("jim",map show [1..4])
    ]

icons :: XMonad.Query [String]
icons = composeAll
    [ className =? "Discord" -->   appIcon "\xfb6e"
    , className =? "Chromium-browser" -->   appIcon "\xf268"
    , className =? "Firefox" -->   appIcon "\63288"
    , className =? "Spotify" <||>  className =? "spotify" -->   appIcon "阮"
    , className =? "jetbrains-idea" -->   appIcon "\xe7b5"
    , className =? "Skype" -->   appIcon "\61822"
    , ("nvim" `isPrefixOf`) <$> title -->   appIcon "\59333"
    ]



-- Colors
mainColor = "#FFEBEF"
secondaryColor = "#8BB2C1"
tertiaryColor  = "#A3FFE6"

myTerm       = "alacritty"
myBorderWidth  = 2

myTabConfig = def { inactiveBorderColor = "#FF0000"
                  , activeTextColor = "#00FF00"}
myLayout =
  smartBorders $
  spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
  avoidStruts $
  onWorkspace "9" simpleFloat $
  tiled
  ||| Mirror tiled
  ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion
    -- Move focus to the next window
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100


myManageHook = composeAll
    [ className =? "Firefox"        --> doShift (myWorkspaces !! 1)
    , className =? "Gimp"           --> doFloat
    , className =? "Steam" --> doFloat
    , className =? "steam" --> doFloat
    , className =? "jetbrains-idea" --> doFloat
    , className =? "Spotify" --> doShift "4"
    , className =? "Ghidra" --> doFloat
    , className =? "zoom" --> doFloat
    , ("Minecraft" `isPrefixOf`) <$> className  --> doFullFloat
    , namedScratchpadManageHook scratchpads
    , manageDocks
    , className =? "ableton live 10 lite.exe" --> doFloat
--    , className =? "Xmessage" --> doFloat
--    , workspaceSetHook workspaceSets
    ]



type NamedScratchPadSet = [(String,NamedScratchpad)]
scratchpadSet :: [(String,NamedScratchpad)]
scratchpadSet =
            [ ("M-C-s", NS "spotify" "spotify" (className =? "Spotify") defaultFloating )
            , ("M-C-d", NS "discord" "discocss" (("discord" `isSuffixOf`) <$> className) nonFloating )
            , ("M-C-m", NS "skype" "skypeforlinux" (className =? "Skype") defaultFloating )
            , ("M-C-t", NS "teams" "teams" (className =? "Microsoft Teams - Preview") nonFloating )
            , ("M-C-a", NS "authy" "authy" (className =? "Authy Desktop") defaultFloating )
            ]

getScratchPads :: NamedScratchPadSet -> NamedScratchpads
getScratchPads = map snd

getScratchPadKeys :: NamedScratchPadSet -> [(String,X ())]
getScratchPadKeys ns = map mapFunc ns
    where mapFunc (key,NS {name = name'}) = (key,namedScratchpadAction ns' name')
          ns' = getScratchPads ns

scratchpads = getScratchPads scratchpadSet
scratchpadKeys = getScratchPadKeys scratchpadSet

appKeys= [("M-S-r", spawn "rofi -show combi")
        -- Start alacritty
        ,("M-S-t", spawn myTerm)
        --Take screenshot
        ,("M-S-s", spawn "~/.xmonad/screenshot-sec.sh")
        --Chrome
        ,("M-S-g", spawn "google-chrome-stable")
        --Start emacs
        ,("M-d", spawn "emacsclient -c")
        ]


myKeys =  concat
            [ scratchpadKeys
            , multiScreenKeys
            , appKeys
            , customKeys
--            , xineramaKeys
--            , workspaceKeys
            ]

multiScreenKeys = [("M"++m++key, screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ["-w","-e"] [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "-S")]]

customKeys =
    [ ("<XF86AudioPrev>",dbusAction previous )
    , ("<XF86AudioNext>", dbusAction next)
    , ("<XF86AudioPlay>", dbusAction playPause)
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    --  Reset the layouts on the current workspace to default
    -- Swap the focused and the master window
    -- Polybar toggle
    , ("M-b", spawn "polybar-msg cmd toggle" )
--    , ("M-r", promptSearchBrowser promptConfig "chromium" hoogle )
    , ("M-r", shellPrompt promptConfig )
    , ("M-m", nextWSSet True )
    , ("M-n", prevWSSet True)
    , ("M-S-m" ,  moveToNextWsSet True)
    , ("M-S-n" , moveToPrevWsSet True)

--    , ("M1-<Tab>", nextWS )
--    , ("M1-S-<Tab>", prevWS)
    ]

xineramaKeys = [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

promptConfig = (def {
        fgColor = mainColor,position = CenteredAt 0.3 0.5
        , font = "xft:Hasklug Nerd Font:style=Regular:size=12"
        })

workspaceKeys =
    [ ("M-l",nextWS)
    , ("M-h",prevWS)
    ]
