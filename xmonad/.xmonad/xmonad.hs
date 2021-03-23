-- Base
import XMonad
import System.Exit
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)

-- Data
import qualified Data.Map        as M
import Data.Monoid

-- Hooks
import XMonad.Hooks.ManageDocks -- Avoid xmobar beign hidden under windows
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog

-- Layouts

-- Layouts Modifiers
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)

-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

  -- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

--
myNewKeys =
    [
        --XMonad
      ("M-C-r", spawn "xmonad --recompile"  ) -- recompiles xmonad
      ,("M-r", spawn "xmonad --restart"  )  --restarts xmonad
      , ("M-S-q", io exitSuccess)             -- Quits xmonad
      -- Launchers
      ,("M-<Return>", spawn (myTerminal)) -- terminal
      ,("M-d", spawn "rofi -show run") -- dmenu
      ,("M-<Space>", spawn "rofi -modi 'drun' -show drun") --app launcher
      -- ,("M-<F2>", spawn "google-chrome-stable")-- google chrome
      ,("M-<F2>", spawn "brave")-- brve browser
      ,("M-<F1>", spawn "pcmanfm")-- brve browser
      -- Kill windows
      ,("M-q", kill1)
      --layouts
      ,("M-w", sendMessage NextLayout)
      --Floating windows
      ,("M-S-<Space>",  withFocused $ windows . W.sink     )  -- force back to tiling
      , ("M-C-S-<Space>", sinkAll)                       -- Push ALL floating windows to tile
      -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack
        -- window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane

        -- full screen
        -- , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

        -- Multimedia  keys
        ,("<XF86MonBrightnessUp>", spawn "myBacklight -inc")
        ,("<XF86MonBrightnessDown>", spawn "myBacklight -dec")

        -- prompts
        ,("M-c", spawn "quickconfig")

    ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [

    -- Resize viewed windows to the correct size
     -- ((modm,               xK_n     ), refresh)

    -- Shrink the master area
    -- , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    -- , ((modm,               xK_l     ), sendMessage Expand)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- tall = renamed [Replace "tall"]
--             $ windowNavigation

myLayout =   smartBorders $ avoidStruts ( tiled ||| Mirror tiled ||| noBorders Full )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 65/100

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ] <+> manageDocks <+> composeOne [
                     isFullscreen  -?> doFullFloat]

myEventHook = mempty

-- myLogHook = return ()

myStartupHook = do
  spawnOnce "nitrogen --set-zoom-fill --random ~/Pictures/WallpapersDev/ &"
  spawnOnce "picom -b &"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
  spawnOnce "deadd-notification-center &"
  spawnOnce "batteryAlert.sh &"
  -- spawnOnce "emacs --daemon &"
  spawnOnce "traylaunch.sh & "
  spawnOnce " trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 140 --tint 0x666666 --height 40 --iconspacing 7 &"
  spawnOnce "exec xinput --set-prop 'SYNA1D31:00 06CB:CD48 Touchpad' 'libinput Accel Speed' 0.7"
  spawnOnce "exec xinput --set-prop 'SYNA1D31:00 06CB:CD48 Touchpad' 'libinput Tapping Enabled' 1"

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar"
  xmonad $ fullscreenSupport $ docks  def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            =   dynamicLogWithPP xmobarPP
                        { ppOutput =  hPutStrLn xmproc0
                        , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#98be65" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"          -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppOrder  = \(l:ws:t:ex) -> [ws,l]++ex++[t]
                        },

        startupHook        = myStartupHook
    }`additionalKeysP` myNewKeys
