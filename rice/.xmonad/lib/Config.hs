module Config (myConfig) where
import           Control.Monad
import           Control.Monad                       (forM_, join)
import           Data.Function                       (on)
import           Data.List                           (sortBy)
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
import           XMonad.Util.Run
import           XMonad.Util.Run                     (safeSpawn)
import           Polybar (polybarPP)
import           DynamicLog
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Hooks.ServerMode
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig
import           XMonad.Prompt.XMonad

myStartupHook = do
     io $ forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
     spawn "/bin/sh ~/.xmonad/polybar.sh"
     spawnOnce "discord"
     spawn "feh --bg-center ~/background3.png"
     spawn "picom --experimental-backend --config=.config/picom/picom.conf"

--Conf
myWorkspaces =  ["1:\xf015 ","2:\62056 ","3:\61728 ","4:\61884 "] ++ map show [5..9]
myConfig = ewmh $ def {
       terminal = myTerm
      , borderWidth = myBorderWidth
      , normalBorderColor = secondaryColor
      , focusedBorderColor = mainColor
      , workspaces = myWorkspaces
      , keys = customKeys
      , modMask = mod1Mask
      , focusFollowsMouse = False
      , startupHook = myStartupHook
      , logHook = dynamicLogWithPP polybarPP
      , manageHook = (isFullscreen --> doFullFloat) <> manageDocks <>  manageHook def
      , layoutHook =  myLayout
      , handleEventHook = serverModeEventHook <> handleEventHook def <> docksEventHook <> fullscreenEventHook }





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
    , className =? "Steam" --> doFloat
    , className =? "discord" --> doShift (myWorkspaces !! 2)
    , className =? "jetbrains-idea" --> doFloat
    , className =? "Spotify" --> doShift "4"
    ]

customKeys conf@(XConfig {XMonad.modMask = modm}) = mkKeymap conf $
    [
    --Start rofi
    ("M-S-r", spawn "rofi -show combi")
    -- Start alacritty
	,("M-S-t", spawn $ terminal conf)
	-- Kill currently focused window
	,("M-S-c",kill)
	--Take screenshot
	,("M-S-s", spawn "~/.xmonad/screenshot-sec.sh")
	--Chrome
	,("M-S-g", spawn "firefox")
    --Start vim
    ,("M-d", spawn "st -t NEOVIM nvim")

	--- Multimedia keys
	,("<XF86AudioPrev>", spawn "playerctl previous")
	,("<XF86AudioNext>", spawn "playerctl next")
	,("<XF86AudioPlay>", spawn "playerctl play-pause")
	,("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
	,("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
	,("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
	--  Reset the layouts on the current workspace to default
	, ("M-s-<Space>", setLayout $ XMonad.layoutHook conf)

	-- Swap the focused and the master window
	, ("M-<Return>", windows $ W.swapMaster . W.focusDown)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)
    -- Move focus to preview window
    , ("M-S-<Tab>",    windows W.focusUp)
    --Polybar toggle
	,("M-b", spawn "echo cmd:toggle | tee /tmp/polybar_mqueue.* >/dev/null" )

	--Push window back into tiling
	, ("M-t", withFocused $ windows . W.sink)
        -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN (-1)))

	, ("M-<Space>", sendMessage NextLayout)
    , ("M-r", shellPrompt (def {fgColor = mainColor,position = CenteredAt 0.3 0.5, font = "xft:Inconsolata Nerd Font:style=Regular:size=12"  })  )
	] ++
	-- Xmonad keys
	[
	("M-q", spawn "xmonad --recompile; xmonad --restart" )
	,("M-S-q", io exitSuccess)
	] ++


	--Workspace keys
	[("M"++m++'-':show k, windows $ f i)
	    | (i, k) <- zip (XMonad.workspaces conf) [1..9]
            , (f, m) <- [(W.greedyView, ""), (W.shift, "-S")]]
	++
	 [("M"++m++key, screenWorkspace sc >>= flip whenJust (windows . f))
	    | (key, sc) <- zip ["-w","-e"] [0..]
	    , (f, m) <- [(W.view, ""), (W.shift, "-S")]]

