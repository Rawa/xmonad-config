-- xmonad config used by David Göransson
-- Author: David Göransson
-- This config is mostly a mashup from Vic Fryzel's (https://github.com/vicfryzel) 
-- and robertmassaioli's (https://bitbucket.org/robertmassaioli) xmonad configs.
-- Most changes that have been made are to better the config for laptop usage.
-- 
-- http://github.com/rawa/xmonad-config


--
-- Imports needed for config
--
import System.IO
import System.Exit
import XMonad
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.TagWindows
import XMonad.Actions.OnScreen (greedyViewOnScreen, viewOnScreen)
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.IndependentScreens
import XMonad.Util.WindowProperties
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run
import XMonad.Util.Dmenu
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Prompt
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.StackSet as S
import XMonad.Config.Gnome
import Control.Monad
import Data.Ratio
import Data.Maybe (fromMaybe)

----------------------
-- Define variables --
----------------------
myTerminal = "urxvt"
myBrowser = "google-chrome-stable"
runMenu = spawn "exec $(/home/rawa/.cabal/bin/yeganesh -x)"


-----------------------
-- Define Workspaces --
-----------------------
myWorkspaces = ["1:term","2:web","3:code","4:irc","5:media","6:vm"] ++ map show [7..9]

--
-- Define windows
-- Use 'xprop | grep WM_CLASS' to find the WM_NAME you want to move.
--

myManageHook = composeAll
    ([ className =? "x-www-browser" --> doShift "2:web"
    , className =? "Google-chrome-stable"  --> doShift "2:web"
    , className =? "Subl3"          --> doShift "3:code"
    , className =? "Eclipse"        --> doShift "3:code"
    , className =? "Java"           --> doShift "3:code"
    , title =? "irssi"              --> doShift "4:irc"
    , className =? "spotify.exe"    --> doShift "5:media"
    , className =? "Wine"           --> doShift "5:media"
    , className =? "VirtualBox"     --> doShift "6:vm"
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]) <+> namedScratchpadManageHook myScratchpads


----------------
-- Scratchpads--
----------------

myScratchpads = let 
  reallyFull = customFloating $ W.RationalRect 0.025 0.025 0.95 0.95
  full = customFloating $ W.RationalRect 0.03 0.05 0.94 0.9
  top = customFloating $ W.RationalRect 0.025 0.05 0.95 0.45
  bottom = customFloating $ W.RationalRect 0.1 0.7 0.80 0.3
  in [
    NS "Mail" 
       "google-chrome-stable --app=https://mail.google.com"
       (appName =? "mail.google.com") full 
  , NS "Calendar"
       "google-chrome-stable --app=https://calendar.google.com"
       (appName =? "calendar.google.com") full
  , NS "BottomTerminal"
       "urxvt -name BottomTerminal"
       (appName =? "BottomTerminal") bottom 
  ]

------------------------
-- Layout definitions --
------------------------

-- The layouts which xmonad switches between
myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig) |||
    -- full |||
    noBorders (fullscreenFull Full)

---------------------------------
-- Color and design properties --
---------------------------------


-- Define the colors and borders of windows in xmonad

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.

tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "green",
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



------------------
-- Key bindings --
------------------

-- Setting windows button as primary action (mod)
myModMask = mod4Mask
-- Alt as secondary action.
altMask   = mod1Mask

-- Key binds
myKeys conf = M.fromList $
    [ ((myModMask              , xK_Return    ), spawn myTerminal)
    , ((myModMask              , xK_x         ), spawn myBrowser)
    , ((myModMask              , xK_space     ), runMenu)
    , ((myModMask              , xK_c         ), kill)
    -- Empty Workspace Movement
    , ((altMask                , xK_r         ), viewEmptyWorkspace)
    , ((altMask .|. shiftMask  , xK_r         ), tagToEmptyWorkspace)
    -- Layout Commands
    , ((altMask                , xK_space     ), sendMessage NextLayout)
    , ((altMask .|. shiftMask  , xK_Return    ), sendMessage FirstLayout)
    , ((myModMask              , xK_g         ), refresh)
    , ((myModMask              , xK_f         ), windows S.swapMaster)
    -- controlling window movement, position and location

    , ((myModMask              , xK_m         ), namedScratchpadAction myScratchpads "Mail")
    , ((myModMask              , xK_z         ), namedScratchpadAction myScratchpads "BottomTerminal")
    , ((myModMask              , xK_n         ), namedScratchpadAction myScratchpads "Calendar")
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
    , ((myModMask              , xK_h         ), spawn "sudo pm-hibernate")
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

--------------------
-- Mouse bindings --
--------------------

-- Focus rules
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True


-- Movements of windows, user mod + t to reset tile.
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
  ]

------------------
-- Startup hook --
------------------
myStartupHook = return ()


----------------
-- Run xmonad --
----------------
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = " | "
          , ppSort = fmap 
                              (namedScratchpadFilterOutWorkspace.)
                              (ppSort defaultPP)
      }
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
  }


------------------
-- Merge it all --
------------------
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
