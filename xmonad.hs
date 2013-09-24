-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.StackSet as S

import XMonad
import XMonad.Util.EZConfig

import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Util.WindowProperties
import Control.Monad
import Data.Ratio
import qualified Data.Map as M
import XMonad.Util.Run
import XMonad.Util.Dmenu
--import XMonad.Hooks.DynamicLog (dynamicLogXinerama, xmobar)
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Layout.IndependentScreens
import XMonad.Actions.Warp
import XMonad.Hooks.UrgencyHook

import XMonad.Prompt
import XMonad.Actions.TagWindows

import XMonad.Actions.OnScreen (greedyViewOnScreen, viewOnScreen)
import Data.Maybe (fromMaybe)

import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/gnome-terminal"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:term","2:web","3:code","4:subl","5:media","6:vm"] ++ map show [7..9]


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "x-www-browser"  --> doShift "2:web"
    , className =? "Google-chrome"  --> doShift "2:web"
    , className =? "Sublime_text"   --> doShift "4:subl"
    , className =? "Eclipse"        --> doShift "3:code"
    , className =? "Java" 	        --> doShift "3:code"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "VirtualBox"     --> doShift "6:vm"
    , className =? "Xchat"          --> doShift "5:media"
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)


------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "green"

-- Width of the window border in pixels.
myBorderWidth = 1

myBrowser = "x-www-browser"   
--myTerminal = "x-terminal-emulator"
runMenu = spawn "dmenu_run"

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask
altMask   = mod1Mask

myKeys conf = M.fromList $
    [ ((myModMask              , xK_Return    ), spawn myTerminal)
    , ((myModMask              , xK_x         ), spawn myBrowser)
    , ((myModMask              , xK_r         ), runMenu)
    , ((myModMask              , xK_c         ), kill)
    -- Empty Workspace Movement
    , ((altMask                , xK_space     ), viewEmptyWorkspace)
    , ((altMask .|. shiftMask  , xK_space     ), tagToEmptyWorkspace)
    -- Layout Commands
    , ((myModMask              , xK_space     ), sendMessage NextLayout)
    , ((altMask .|. shiftMask  , xK_Return    ), sendMessage FirstLayout)
    , ((myModMask              , xK_n         ), refresh)
    , ((myModMask              , xK_m         ), windows S.swapMaster)
    -- controlling window movement, position and location
    , ((altMask                , xK_Tab       ), windows S.focusDown)
    , ((altMask .|. shiftMask  , xK_Tab       ), windows S.focusUp)
    , ((myModMask              , xK_Tab       ), windows S.focusDown)
    , ((myModMask .|. shiftMask, xK_Tab       ), windows S.focusUp)
    , ((myModMask              , xK_Down      ), windows S.swapDown)
    , ((myModMask              , xK_Up        ), windows S.swapUp)
    , ((myModMask              , xK_Left      ), sendMessage Shrink)
    , ((myModMask              , xK_Right     ), sendMessage Expand)
    , ((myModMask              , xK_t         ), withFocused $ windows . S.sink)
    , ((myModMask              , xK_w         ), sendMessage (IncMasterN 1))
    , ((myModMask              , xK_v         ), sendMessage (IncMasterN (-1)))
    -- Shutdown commands
    , ((myModMask              , xK_q         ), restart "xmonad" True)
    , ((myModMask              , xK_h         ), spawn "gksudo pm-hibernate")
    , ((myModMask .|. shiftMask, xK_q         ), io (exitWith ExitSuccess))
    , ((myModMask .|. shiftMask, xK_w         ), spawn "gnome-screensaver-command -l")
    -- Print Screen
    , ((myModMask              , xK_Print     ), spawn "gnome-screenshot")
    , ((altMask                , xK_Print     ), spawn "gnome-screenshot -a")
    -- MPC and Volume commands
    , ((myModMask               , xK_Page_Up  ), spawn "mpc next")
    , ((myModMask               , xK_Page_Down), spawn "mpc prev")
    , ((myModMask               , xK_Pause    ), spawn "mpc toggle")
    , ((myModMask               , xK_F12      ), spawn "amixer -q set Master 5%+")
    , ((myModMask               , xK_F11      ), spawn "amixer -q set Master 5%-")
    , ((myModMask               , xK_F10      ), spawn "amixer -D pulse set Master 1+ toggle")
    , ((0                       , 0x1008ff12  ), spawn "amixer -D pulse set Master 1+ toggle")
    , ((0                       , 0x1008ff11  ), spawn "amixer -q set Master 5%-")
    , ((0                       , 0x1008ff13  ), spawn "amixer -q set Master 5%+")

    ] ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. myModMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = " | "
      }
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
  }


------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
  }
