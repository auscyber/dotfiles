{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Config (myConfig,ExtraState) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Functor
import           Data.Function                       (on)
import           Data.List                           (nub,nubBy,isSuffixOf, elemIndex )
import qualified Data.Map                            as M
import           Polybar
import           System.Exit
import           XMonad
import           XMonad.Hooks.EwmhDesktops 
import           XMonad.Hooks.ManageDocks 
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.Fullscreen            as F
import           XMonad.Layout.Gaps
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders 
import           XMonad.Layout.Tabbed 
import qualified XMonad.StackSet                     as W
import           XMonad.Util.SpawnOnce
import           XMonad.Util.NamedWindows            (getName)
import           XMonad.Actions.CycleWS
import           XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.NamedScratchpad
import           DynamicLog
import           XMonad.Hooks.DynamicLog
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Hooks.ServerMode
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig
import           XMonad.Actions.Search
import           XMonad.Prompt.Window
import           XMonad.Actions.Commands
import           XMonad.Util.Cursor
import           DBus
import           DBus.Client
import           Media
import           ExtraState
import           WorkspaceSet

myStartupHook = do
     io $ forM_ [".xmonad-workspace-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
     spawn "~/.config/polybar/launch.sh"
     spawn "xrandr --output DP-0 --off --output DP-1 --off --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --mode 1920x1080 --pos 0x0 --rotate normal --output USB-C-0 --off"
--     setDefaultCursor xC_left_ptr 
     spawn "feh --bg-fill ~/background3.png"

dbusAction :: (Client -> IO ()) -> X ()
dbusAction action =  XS.gets dbus_client >>= (id >=> io . action)


commandsX :: X [(String, X ())]
commandsX = asks config Data.Functor.<&> commands
commands conf = [
    ("prevws",prevWS)
    ,("nextws",nextWS)]
    ++ [(m++show k, windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [1..9]
                , (f, m) <- [(W.greedyView, "view"), (W.shift, "moveTo")]]

getWorkspaceText :: M.Map Int String -> String -> String
getWorkspaceText xs n =
    case M.lookup (read n) xs of
        Just x -> x
        _ -> n

myWorkspaces = map show [1..9]


removedKeys :: [String]
removedKeys = ["M-p","M-S-p" , "M-S-e","M-S-o","M-b" ]
createKeybinds :: [(WorkspaceSetId,[WorkspaceId])] -> [((KeyMask,KeySym),X ())]
createKeybinds wsSets = map f keySets
    where totalws = map (uncurry changeWorkspaces) (("default",workspaces myConfig): wsSets)
          keySets = nub $ concatMap (map fst . snd) totalws
          f key = createWsKeybind key $ [(wsId, action) | (wsId,actions) <- totalws,(key',action) <- actions,key' == key] 
myConfig = 
    flip additionalKeys (createKeybinds workspaceSets) $
    flip additionalKeysP myKeys $ 
    flip removeKeysP removedKeys $
    ewmhFullscreen $ def {
       terminal = myTerm
      , borderWidth = myBorderWidth
      , normalBorderColor = secondaryColor
      , focusedBorderColor = mainColor
      , workspaces = myWorkspaces
      , modMask = mod4Mask
      , focusFollowsMouse = False
      , startupHook = myStartupHook
--      , logHook = dynamicLogWithPP (polybarPP workspaceSymbols )
      , logHook = myLogHook
      , manageHook = myManageHook 
      , layoutHook =  myLayout
      , handleEventHook = serverModeEventHookCmd' (liftM2 (<>) commandsX defaultCommands)  <> handleEventHook def 
      <> docksEventHook 
      }

myLogHook :: X ()
myLogHook = do
            wsNames <- XS.gets workspaceNames 
            let pp = polybarPP wsNames 
            --dynamicLogIconsConvert (iconConfig pp)  >>= DynamicLog.dynamicLogString . switchMoveWindowsPolybar . namedScratchpadFilterOutWorkspacePP>>=  io . ppOutput pp
            filterOutInvalidWSet (polybarPP M.empty) >>= DynamicLog.dynamicLogString   >>= io . ppOutput pp

workspaceSets :: [(WorkspaceSetId,[WorkspaceId])]
workspaceSets = 
    [ ("bob",["hi","no"] <> map show [1..5])
    , ("jim",map show [1..4])
    ]

iconConfig :: PP -> IconConfig 
iconConfig pp = def { iconConfigPP = pp, iconConfigIcons = icons }

icons :: IconSet
icons = composeAll [
    className =? "discord" --> appIcon "\xfb6e" 
    , className =? "Chromium-browser" --> appIcon "\xf268"
    , className =? "Firefox" --> appIcon "\63288"
    , className =? "Spotify" <||>  className =? "spotify" --> appIcon "阮"
    , className =? "jetbrains-idea" --> appIcon "\xe7b5" 
    , className =? "Skype" --> appIcon "\61822" 
    , ("vim" `isPrefixOf`) <$> title --> appIcon "\59333"]



-- Colors
mainColor = "#ffc3b6"
secondaryColor = "#FFFBB8"
tertiaryColor  = "#A3FFE6"

myTerm       = "alacritty"
myBorderWidth  = 2

myTabConfig = def { inactiveBorderColor = "#FF0000"
                  , activeTextColor = "#00FF00"}
myLayout =
  smartBorders $
  spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
  avoidStruts $
  tiled
  ||| Mirror tiled
  ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes 
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportio
    -- Move focus to the next window
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100


myManageHook = composeAll
    [ className =? "Firefox"        --> doShift (myWorkspaces !! 1)
    , className =? "Gimp"           --> doFloat
    , className =? "discord" --> doShift (myWorkspaces !! 2)
    , className =? "Steam" --> doFloat
    , className =? "steam" --> doFloat
    , className =? "jetbrains-idea" --> doFloat
    , className =? "Spotify" --> doShift "4"
    , className =? "Ghidra" --> doFloat
    , ("Minecraft" `isPrefixOf`) <$> className  --> doFullFloat 
    , namedScratchpadManageHook scratchpads
    , manageDocks 
    , workspaceSetHook workspaceSets
    ]



type NamedScratchPadSet = [(String,NamedScratchpad)]
scratchpadSet :: [(String,NamedScratchpad)]
scratchpadSet = 
            [ ("M-C-s", NS "spotify" "spotify" (className =? "Spotify") defaultFloating )
            , ("M-C-d", NS "discord" "Discord" (("discord" `isSuffixOf`) <$> className) defaultFloating )
            , ("M-C-m", NS "skype" "skypeforlinux" (className =? "Skype") defaultFloating )
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
        -- Kill currently focused window
        ,("M-S-c",kill)
        --Take screenshot
        ,("M-S-s", spawn "~/.xmonad/screenshot-sec.sh")
        --Chrome
        ,("M-S-g", spawn "chromium")
        --Start emacs
        ,("M-d", spawn "emacsclient -c")

        ]


myKeys =  concat
            [ scratchpadKeys
            , multiScreenKeys
            , appKeys
            , customKeys ]

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
    , ("M-r", promptSearch (def {fgColor = mainColor,position = CenteredAt 0.3 0.5, font = "xft:Hasklug Nerd Font:style=Regular:size=12"  }) hoogle  )
    , ("M-m", XS.modify nextWorkspaceSet >> (join $ asks (logHook . config) ))
    , ("M-n",XS.modify previousWorkspaceSet >> (join $ asks (logHook . config)))

    ] ++
    -- Xmonad keys
    [ ("M-l",nextWS)
    , ("M-h",prevWS)
    ]


    
