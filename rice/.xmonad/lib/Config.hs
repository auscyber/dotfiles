{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Config (myConfig,NameSet,ExtraState) where

import           Control.Monad
import           Data.Functor
import           Data.Function                       (on)
import           Data.List                           (isSuffixOf, elemIndex )
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


myStartupHook = do
     io $ forM_ [".xmonad-workspace-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
     spawn "~/.config/polybar/launch.sh"
     spawn "xrandr --output DP-3  --left-of HDMI-0"
--     setDefaultCursor xC_left_ptr 
     spawnOnce "DiscordCanary"
     spawn "feh --bg-fill ~/background3.png"

dbusAction :: (Client -> IO ()) -> X ()
dbusAction action =  XS.gets dbus_client >>= (id >=> io . action)


--nextWS :: X ()
--nextWS = gets (W.currentTag . windowset) >>= \tag -> asks (XMonad.workspaces . XMonad.config ) >>= \ws -> windows . W.greedyView  $  nextTag tag ws
--    where   nextTag :: WorkspaceId -> [WorkspaceId] -> WorkspaceId
--            nextTag tag ws = let Just index = tag `elemIndex` ws in if index == (length ws - 1) then head ws else ws !! (index+1)
--prevWS :: X ()
--prevWS = 
--    gets (W.currentTag . windowset) >>= \tag -> asks (XMonad.workspaces . XMonad.config ) >>= \ws -> windows . W.greedyView $ prevTag tag ws
--    where 
--        prevTag tag ws = let Just index = tag `elemIndex` ws in if index == 0 then last ws else ws !! (index-1)

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

myConfig = 
    flip additionalKeysP myKeys $ 
    flip removeKeysP removedKeys $
    ewmh $ def {
       terminal = myTerm
      , borderWidth = myBorderWidth
      , normalBorderColor = secondaryColor
      , focusedBorderColor = mainColor
      , workspaces = myWorkspaces
      , modMask = mod4Mask
      , focusFollowsMouse = False
      , startupHook = myStartupHook
--      , logHook = dynamicLogWithPP (polybarPP workspaceSymbols )
      , logHook = do
            wsNames <- XS.gets workspaceNames 
            let pp = polybarPP wsNames 
            dynamicLogIcons' iconConfig pp  >>= DynamicLog.dynamicLogString . switchMoveWindowsPolybar >>=  io . ppOutput pp
      , manageHook = myManageHook 
      , layoutHook =  myLayout
      , handleEventHook = serverModeEventHookCmd' (liftM2 (<>) commandsX defaultCommands)  
      <> handleEventHook def 
      <> docksEventHook 
      <> fullscreenEventHook }

baseIconSet :: String -> XMonad.Query [WorkSpaceIconSet ]
baseIconSet str = pure [NameSet { current = str, hidden = str, visible = str, showOverlayIcons = True } ]

iconConfig :: IconConfig 
iconConfig = def { iconConfigIcons = icons }

icons :: XMonad.Query [WorkSpaceIconSet]
icons = composeAll [
    className =? "discord" --> baseIconSet "\xfb6e" 
    , className =? "Chromium-browser" --> baseIconSet "\xf268"
    , className =? "Firefox" --> baseIconSet "\63288"
    , className =? "Spotify" <||>  className =? "spotify" --> baseIconSet "阮"
    , className =? "jetbrains-idea" --> baseIconSet "\xe7b5" 
    , className =? "Skype" --> baseIconSet "\61822" ]



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
    ]



type NamedScratchPadSet = [(String,NamedScratchpad)]
scratchpadSet :: [(String,NamedScratchpad)]
scratchpadSet = 
            [ ("M-C-s", NS "spotify" "spotify" (className =? "Spotify") defaultFloating )
            , ("M-C-d", NS "discord" "DiscordCanary" (("discord" `isSuffixOf`) <$> className) defaultFloating )
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
    ] ++
    -- Xmonad keys
    [ ("M-l",nextWS)
    , ("M-h",prevWS)
    ]


    --Workspace keys
    
