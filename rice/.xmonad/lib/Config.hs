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
import           XMonad.Util.NamedWindows            (getName)
import           XMonad.Util.Run
import           XMonad.Util.Run                     (safeSpawn)
import Polybar (polybarPP)
import DynamicLog


myStartupHook = do
     io $ forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
     spawn "/bin/sh ~/.xmonad/polybar.sh"
     spawn "discord"
     spawn "picom --config=.config/picom/picom.conf"

--Conf
myWorkspaces =  ["1:\xf015 ","2:\62056 ","3:\61728 ","4:\61884 "] ++ map show [5..9]
myConfig = ewmh $ def {
       terminal = myTerm
      , borderWidth = myBorderWidth
      , normalBorderColor = secondaryColor
      , focusedBorderColor = mainColor
      , workspaces = myWorkspaces
      , keys = customKeys
      , modMask = mod4Mask
      , focusFollowsMouse = False
      , startupHook = myStartupHook
      , logHook = dynamicLogWithPP polybarPP
      , manageHook = (isFullscreen --> doFullFloat) <+> manageDocks <+>  manageHook def
      , layoutHook =  myLayout
      , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook }





-- Colors
mainColor = "#ffc3b6"
secondaryColor = "#FFFBB8"
tertiaryColor  = "#A3FFE6"

myTerm       = "st"
myBorderWidth  = 2

--Polybar


myTabConfig = def { inactiveBorderColor = "#FF0000"
                  , activeTextColor = "#00FF00"}
myLayout =
  avoidStruts $
  smartBorders $
  tiled
  ||| Mirror tiled
  ||| Full
  ||| tabbed shrinkText myTabConfig

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
    [ className =? "Google-chrome"        --> doShift (myWorkspaces !! 1)
    , className =? "Gimp"           --> doFloat
    , className =? "Steam" --> doFloat
    , className =? "discord" --> doShift (myWorkspaces !! 2)
    , className =? "jetbrains-idea" --> doFloat
    , className =? "Spotify" --> doShift "9"
     ]
customKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    -- Start dmenu
    --((modm .|. shiftMask, xK_r ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    ((modm .|. shiftMask, xK_r), spawn "rofi -show combi")
    -- Start st
	,((modm .|. shiftMask, xK_t ), spawn $ XMonad.terminal conf)
	-- Kill currently focused window
	,((modm .|. shiftMask, xK_c),kill)
	--Take screenshot
	,((mod4Mask .|. shiftMask, xK_s), spawn "~/.xmonad/screenshot-sec.sh")
	--Chrome
	,((modm .|. shiftMask, xK_g), spawn "google-chrome-stable")
	--- Multimedia keys
	,((0,0x1008ff16), spawn "playerctl previous")
	,((0,0x1008ff17), spawn "playerctl next")
	,((0,0x1008ff14), spawn "playerctl play-pause")
	,((0,0x1008ff13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
	,((0,0x1008ff11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
	,((0,0x1008ff12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
	--  Reset the layouts on the current workspace to default
	, ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
	,((modm, xK_Caps_Lock), sendMessage $ Toggle FULL)

	-- Swap the focused and the master window
	, ((modm,               xK_Return), windows $ W.swapMaster . W.focusDown)

    -- Move focus to the next window
    , ((mod1Mask,               xK_Tab   ), windows W.focusDown)
    -- Move focus to preview window
    , ((mod1Mask .|. shiftMask, xK_Tab),    windows W.focusUp)
    --Polybar toggle
	,((modm, xK_b     ), spawn "echo cmd:toggle | tee /tmp/polybar_mqueue.* >/dev/null" )

	--Push window back into tiling
	, ((modm,               xK_t     ), withFocused $ windows . W.sink)
        -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

	, ((modm,               xK_space ), sendMessage NextLayout)

	] ++
	-- Xmonad keys
	[
	((modm, xK_q ), spawn "xmonad --recompile; xmonad --restart" )
	,((modm .|. shiftMask,xK_q), io exitSuccess)
	] ++


	--Workspace keys
	[((m .|. modm, k), windows $ f i)
	    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
	++
	 [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
	    | (key, sc) <- zip [xK_e, xK_w] [0..]
	    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



