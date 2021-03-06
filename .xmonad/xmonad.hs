-- Kishore's Xmonad Config
-- github.com/KSatheeskumar21

-- XMonad Base
import XMonad
import Data.Maybe
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
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- Xmonad 0.17
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
import XMonad.Layout.ShowWName
import XMonad.Layout.LayoutModifier

-- Actions
import XMonad.Actions.GroupNavigation
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import System.Posix (BaudRate(B600))

-- Preferred Terminal
myTerminal      = "kitty"
altTerminal     = "st"

-- Preferred Run launcher (Currently using Rofi, probably won't switch back to dmenu, except I've switched back to dmenu)
myLauncher = "dmenu_run -p 'Run:' -h 24"
-- myLauncher = "rofi -show run -display-run 'Run:'"

-- Preferred Browser Program
myBrowser = "brave"

-- Preferred Text editor
myEditor = "emacsclient -c -a 'emacs'"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use.
myModMask       = mod4Mask
altMask         = mod1Mask

-- Workspace Names
myWorkspaces :: [String]
myWorkspaces    = ["dev","www","sys","doc","vbox","chat","mus","vid","gfx"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Border colors for unfocused and focused windows, respectively.

myNormalBorderColor  = "#cccccc"
myFocusedBorderColor = "#B62D65"

------------------------------------------------------------------------
-- Key bindings.

myKeys :: [(String, X ())]
myKeys =

    -- KB_BEGIN
    
    -- KB_GROUP -> Window management
    -- Rotate through the available layout algorithms
    [ ("M-<Space>", sendMessage NextLayout)

    -- Resize viewed windows to the correct size
    , ("M-n", refresh)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)

    -- Move focus to previous window
    , ("M-S-<Tab>", windows W.focusUp)

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
    , ("M-h", sendMessage Shrink)

    -- Expand the master area
    , ("M-l", sendMessage Expand)

    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    -- , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    -- , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    
    -- KB_GROUP -> Applications
    -- Spawnn Kitty
    , ("M-<Return>", spawn myTerminal)

    -- launch dmenu
    , ("M-S-<Return>", spawn myLauncher)

    -- close focused window
    , ("M-S-c", kill)

    -- Spawn Emacsclient
    , ("M-S-e", spawn myEditor)

    -- Spawn PcManFM
    , ("M-S-f", spawn "pcmanfm")
    
    -- Spawns preferred browser ( using Brave, might switch to LibreWolf, keeping a separate binding for Suckless surf )
    , ("M-S-b", spawn myBrowser)
    , ("M1-S-b", spawn "surf-open")

    -- KB_GROUP -> dmscripts
    , ("M-p h", spawn "dm-hub")
    , ("M-p b", spawn "dm-setbg")
    , ("M-p S-b", spawn "dm-bookman")
    , ("M-p c", spawn "dm-confedit")
    , ("M-p i", spawn "dm-ip")
    , ("M-p s", spawn "dm-maim")
    , ("M-p k", spawn "dm-kill")
    , ("M-p q", spawn "dm-logout")
    , ("M-p m", spawn "dm-music")
    , ("M-p S-m", spawn "dm-man")
    , ("M-p S-s", spawn "dm-websearch")
    , ("M-p y", spawn "dm-youtube")

    -- Music Player ( I use dm-music but if I want to visually manage my tracks I use this binding, assuming I'm not using mocp)
    , ("M1-m", spawn (altTerminal ++ " -e ncmpcpp"))

    -- KB_GROUP -> Mocp bindings
    , ("M-m o", spawn (altTerminal ++ " -e mocp"))
    , ("M-m p", spawn "mocp --play")
    , ("M-m P", spawn "mocp --pause")
    , ("M-m u", spawn "mocp --unpause")
    , ("M-m s", spawn "mocp --stop")
    , ("M-m n", spawn "mocp --next")
    , ("M-m N", spawn "mocp --previous")
    
    -- KB_GROUP -> Xmonad
    , ("M-b", sendMessage ToggleStruts)

    -- Quit xmonad
    , ("M-S-x", io exitSuccess)
    , ("M1-S-q", spawn "lxsession-logout")

    -- Restart xmonad
    , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")

    -- Help Script
    , ("M-S-/", spawn "xmonad-keys")
    -- KB_END
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

-- Floating Layout
floatingLayout = renamed [Replace "Floating"] simplestFloat

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True 

-- Golden ratio for Spiral Layout
goldRatio = toRational (2/(1 + sqrt 5 :: Double))
spiralLayout = renamed [Replace "Golden Spiral"] $ mySpacing 5 $ spiral goldRatio

myTabbed = renamed [Replace "Tabbed"] $ simpleTabbed

gridWithGaps = renamed [Replace "Grid"] $ mySpacing 5 $ Grid(16/10)

-- ShowWName Config
showWNameTheme :: SWNConfig
showWNameTheme = def
  { swn_font   = "xft:JetBrains Mono:bold:size=60"
  , swn_fade   = 0.8
  , swn_bgcolor = "#1D252C"
  , swn_color = "#ffffff"
  }

myLayout = mouseResize $ avoidStruts (tall ||| floatingLayout ||| gridWithGaps ||| myTabbed ||| spiralLayout ||| Full)
  where

     -- Tiled layout (ResizableTile)
     tall = renamed [Replace "Tall"] $ mySpacing 8 $ ResizableTall nmaster delta ratio []

     -- default tiling algorithm partitions the screen into two panes
     -- tiledDef   = renamed [Replace "Tall"] $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
------------------------------------------------------------------------
-- Window rules:

-- Mainpulate certain windows, e.g., force it to float, or move it to
-- a different workspace
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

myManageHook = composeAll
    [ className =? "MPlayer"        --> doCenterFloat
    , className =? "Gimp"           --> doCenterFloat
    , className =? "Qalculate-gtk"     --> doCenterFloat
    -- Terminals for music players
    , className =? "st-256color"       --> doShift ( myWorkspaces !! 6 )
    , className =? "Xterm"             --> doShift ( myWorkspaces !! 6 )
    -- GUI Music player
    , className =? "Deadbeef"          --> doShift ( myWorkspaces !! 6 )
    -- Move video players to workspace 8
    , className =? "mpv"               --> doShift ( myWorkspaces !! 7 )
    , className =? "Olivia"            --> doShift ( myWorkspaces !! 7 )
    -- Move browsers to workspace 2
    , className =? "Brave-browser"     --> doShift ( myWorkspaces !! 1 )
    , className =? "firefox"           --> doShift ( myWorkspaces !! 1 )
    , className =? "tabbed-surf"       --> doShift ( myWorkspaces !! 1 )
    , className =? "qutebrowser"       --> doShift ( myWorkspaces !! 1 )
    -- , className =? "Thunar"         --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

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
        -- Set a wallpaper and enable compositor, I have now switched to dmscripts (dm-setbg) to manage my wallpapers
        spawnOnce "nitrogen --restore &"
        -- spawnOnce "xargs xwallpaper --stretch < ~/.cache/wall"
        spawnOnce "picom --experimental-backends &"
        -- Notifications and clipboard
        spawnOnce "dunst &"
        -- spawnOnce "clipcatd"
        spawnOnce "xfce4-clipman &"
        spawnOnce "xclip &"
        -- Music Daemon
        spawnOnce "[ ! -s ~/.config/mpd/pid ] && mpd"
        -- System tray stuff, power management (laptop only)
        spawnOnce "xfce4-power-manager &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x1E1E28 --height 22 &"
        spawnOnce "volumeicon &"
        spawnOnce "nm-applet &"
        -- Polkit app
        spawnOnce "lxsession &"
        -- Setting Mouse Cursor
        spawnOnce "xsetroot -cursor_name left_ptr"
        -- WM name
        setWMName "LG3D"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Runs Xmonad with the settings aforementioned as well as others defined in the 'main' block

-- Xmonad 0.17
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


-- Main block
main = do
       -- Xmonad 0.17 stuff
       -- Xmobar
       -- mySB <- statusBarPipe "/usr/bin/xmobar -x 0 /home/kishore/.config/xmobar/doom-city-lights-xmobarrc" (pure myXmobarPP)
       -- xmonad $ withSB mySB $ ewmh $ docks $ def {
       mySB <- spawnPipe "xmobar -x 0 /home/kishore/.config/xmobar/catppuccin-xmobarrc"
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

        -- hooks, layouts
        layoutHook         = showWName' showWNameTheme $ myLayout,
        manageHook         = myManageHook,
        handleEventHook    = ewmhDesktopsEventHook,
        -- logHook            = myLogHook,
        logHook            = workspaceHistoryHook <+> dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn mySB
                                                     , ppCurrent = xmobarColor "#C6AAE8" "" . wrap "[" "]"
                                                     , ppVisible = xmobarColor "#C6AAE8" "" . clickable
                                                     , ppHidden = xmobarColor "#A4B9EF" "" . wrap "*" "" . clickable
                                                     , ppHiddenNoWindows = xmobarColor "#A4B9EF" "" . clickable
                                                     , ppTitle = xmobarColor "#DADAE8" "" . shorten 60
                                                     , ppSep = "<fc=#666666> | </fc>"
                                                     , ppUrgent = xmobarColor "#E38C8F" "" . wrap "!" "!"
                                                     , ppExtras = [windowCount]
                                                     , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                                     },
        startupHook        = myStartupHook
    } `additionalKeysP` myKeys
