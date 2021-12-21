-- Kishore's Xmonad Config
-- github.com/KSatheeskumar21

-- XMonad Base
import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO

-- Utilties
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.Loggers

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.SetWMName
-- import XMonad.Hooks.StatusBar
-- import XMonad.Hooks.StatusBar.PP

-- Layouts & Layout modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.GridVariants(Grid(Grid))
import XMonad.Layout.Tabbed
import XMonad.Layout.Spiral
import XMonad.Layout.Renamed
import XMonad.Layout.Magnifier
import XMonad.Layout.ResizableTile

-- Actions
import XMonad.Actions.GroupNavigation
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import System.Posix (BaudRate(B600))

-- Prompts
-- import XMonad.Prompt
-- import XMonad.Prompt.AppLauncher
-- import XMonad.Prompt.Man
-- import XMonad.Prompt.Shell

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

-- Preferred Run launcher
myLauncher = "dmenu_run -p 'Run:' -h 24"

-- Preferred Browser Program
-- myBrowser = "brave"
myBrowser = "brave-dev"

-- Preferred Text editor
myEditor = "emacsclient -c -a 'emacs'"
altEditor = "code"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

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
altMask         = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces    = ["dev","www","sys","doc","vbox","chat","mus","vid","gfx"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#cccccc"
myFocusedBorderColor = "#B62D65"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: [(String, X ())]
myKeys =

    -- launch a terminal
    [ ("M-<Return>", spawn myTerminal)

    -- launch dmenu
    , ("M-S-<Return>", spawn myLauncher)

    -- Keybinding Group - Xmonad.Prompt
    -- , ((altMask .|. shiftMask,                 xK_Return), shellPrompt def)
    -- , ((altMask, xK_m), manPrompt def)

    -- close focused window
    , ("M-S-c", kill)

     -- Rotate through the available layout algorithms
    , ("M-<Space>", sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    -- , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ("M-n", refresh)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)

    -- Move focus to the next window
    , ("M-j", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", windows W.focusUp  )

    -- Move focus to the master window
    , ("M-m", windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ("M1-<Return>", windows W.shiftMaster)

    -- Swap the focused window with the next window
    , ("M-S-j", windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ("M-S-k", windows W.swapUp    )

    -- Shrink the master area
    -- , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    -- , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    -- , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    -- , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Spawn Emacsclient
    , ("M-S-e", spawn myEditor)
    -- , ("C-e v", spawn (myEditor ++ ("--eval '(+vterm/toggle nil)'")))

    -- Spawn VSCode
    , ("M1-e", spawn altEditor)
    -- , ("M1-e x", spawn (altEditor ++ ".config/xmonad"))
    -- , ("M1-e a", spawn (altEditor ++ ".config/alacritty"))

    -- Spawn Thunar
    , ("M-S-f", spawn "pcmanfm")

    -- Clipcat-menu
    -- , ("M-S-o", spawn "clipcat-menu")

    -- Spawns brave browser
    , ("M-S-b", spawn myBrowser)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ("M-S-x", io exitSuccess)
    , ("M1-S-q", spawn "lxsession-logout")

    -- Restart xmonad
    , ("M1-S-r", spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ("M-S-/", spawn ("echo \"" ++ help ++ "\" | yad "))
    ]

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    -- [((m .|. modm, k), windows $ f i)
    --    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- My preferred layouts

-- Extra Layouts


-- Golden ratio for Spiral Layout
-- goldRatio = toRational (2/(1 + sqrt 5 :: Double))
-- spiralLayout = renamed [Replace "Golden Spiral"] $ spacing 5 $ spiral goldRatio

-- Floating Layout
floatingLayout = renamed [Replace "Floating"] simplestFloat

myLayout = mouseResize $ avoidStruts (tiled ||| tiledDef ||| floatingLayout ||| emacsLayout ||| Grid(16/10) ||| simpleTabbed ||| Mirror tiledDef ||| Full)
  where

     -- Tiled layout (ResizableTile)
     tiled = renamed [Replace "Resizable M&Stack"] $ ResizableTall nmaster delta ratio []

     -- default tiling algorithm partitions the screen into two panes
     tiledDef   = renamed [Replace "Master and Stack"] $ Tall nmaster delta ratio

     -- Layout for working in Emacs
     emacsLayout = renamed [Replace "EmacsDev"] $ Mirror $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
------------------------------------------------------------------------
-- Window rules:

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
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Qalculate-gtk"     --> doFloat
    -- , className =? "mpv"            --> doFloat
    -- , className =? "Thunar"         --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook :: XConfig a -> XConfig a
-- myEventHook = ewmhDesktopsEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
        -- Spawn the emacs daemon
        spawn "/usr/bin/emacs --daemon &"
        -- Set a wallpaper and enable compositor
        spawnOnce "nitrogen --restore &"
        -- spawnOnce "picom --experimental-backends --config ~/.config/picom/picom.conf &"
        -- spawnOnce "picom --config ~/.config/picom/vm-picom.conf &"
        -- Notifications and clipboard
        spawnOnce "dunst &"
        -- spawnOnce "clipcatd"
        spawnOnce "xfce4-clipman &"
        -- System tray stuff, power management (laptop only)
        spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x1D252C --height 22 &"
        spawnOnce "volumeicon &"
        spawnOnce "nm-applet &"
        spawnOnce "xfce4-power-manager &"
        -- Polkit app
        spawnOnce "lxsession &"
	-- Setting Mouse Cursor
	spawnOnce "xsetroot -cursor_name left_ptr"
        -- WM name
        setWMName "nintenno-xmonad"
        -- spawnOnce "~/.config/polybar/launch.sh"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

-- Xmobar
-- myStatusBarPP = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc0 }

-- myXmobarPP :: PP
-- myXmobarPP = def { ppCurrent = red . wrap "[" "]"
--                 , ppTitleSanitize = xmobarStrip
--                 , ppVisible = blue
--                 , ppHidden = xmobarColor "#82AAFF" "" . wrap "(" ")"
--                 , ppHiddenNoWindows = blue
--                 , ppTitle = white . shorten 60
--                 , ppSep = magenta "<fc=#666666> | </fc>"
--                 , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
--                 , ppExtras = [logTitles formatFocused formatUnfocused]
--                 , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
--                 }
--  where 
--       formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
--       formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

--      ppWindow :: String -> String
--       ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

--       blue, lowWhite, magenta, red, white, yellow :: String -> String
--       magenta = xmobarColor "#ff79c6" ""
--       blue = xmobarColor "#7aa2f7" ""
--       white = xmobarColor "#f8f8f2" ""
--       yellow = xmobarColor "#f1fa8c" ""
--       red = xmobarColor "#F07178" ""
--       lowWhite = xmobarColor "#bbbbbb" ""

main = do
       -- Xmobar
       -- mySB <- statusBarPipe "/usr/bin/xmobar -x 0 /home/kishore/.config/xmobar/doom-city-lights-xmobarrc" (pure myXmobarPP)
       mySB <- spawnPipe "xmobar -x 0 /home/kishore/.config/xmobar/doom-city-lights-xmobarrc"
       -- xmonad $ withSB mySB $ ewmh $ docks $ def {
       xmonad $ ewmh $ docks $ def {
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
        -- keys               = myKeybindings,
        -- mouseBindings      = myMouseBindings,

      -- hooks, layouts
        -- layoutHook         = spacingRaw True (Border 0 8 8 8) True (Border 0 8 8 8) True myLayout,
        -- layoutHook         = gaps [(U, 8), (R, 8)] myLayout,
        layoutHook         = spacingWithEdge 5 myLayout,
        manageHook         = myManageHook,
        -- handleEventHook    = ewmh,
        -- logHook            = myLogHook,
        logHook            = workspaceHistoryHook <+> dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn mySB
                                                     , ppCurrent = xmobarColor "#F07178" "" . wrap "[" "]"
                                                     , ppVisible = xmobarColor "#7aa2f7" ""
                                                     , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
                                                     , ppHiddenNoWindows = xmobarColor "#7aa2f7" ""
                                                     , ppTitle = xmobarColor "#f8f8f2" "" . shorten 60
                                                     , ppSep = "<fc=#666666> | </fc>"
                                                     , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
                                                   -- , ppExtras = [windowCount]
                                                     , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                                     },
        startupHook        = myStartupHook
    } `additionalKeysP` myKeys

