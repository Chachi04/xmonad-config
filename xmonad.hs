-- Base
import XMonad
import System.IO
import System.Exit
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Actions.Navigation2D
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

-- Data
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat, doCenterFloat, isDialog)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Minimize
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import qualified Codec.Binary.UTF8.String as UTF8

-- Layouts
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.IndependentScreens
import XMonad.Layout.CenteredMaster(centerMaster)

-- Layouts modifiers
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Minimize

-- Util
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings, additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Loggers

-- Configs
import XMonad.Config.Desktop
import XMonad.Config.Azerty

import Graphics.X11.ExtraTypes.XF86
import Control.Monad (liftM2)
import qualified DBus as D
import qualified DBus.Client as D

-- Extra
-- import System.Directory
-- import XMonad.Actions.WindowGo (runOrRaise)
-- import XMonad.Actions.WithAll (sinkAll, killAll)
-- import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
-- import XMonad.Actions.Promote
-- import XMonad.Actions.MouseResize
-- import XMonad.Actions.GridSelect
-- import XMonad.Actions.Navigation2D
-- import XMonad.Actions.CopyWindow (kill1)
-- import qualified XMonad.Actions.Search as S
    --
-- import Data.Char (isSpace, toUpper)
-- import Data.Maybe (fromJust, isJust)
-- import Data.Monoid
-- import Data.Tree
-- import qualified Data.ByteString as B
    --
-- import XMonad.Hooks.ServerMode
-- import XMonad.Hooks.WorkspaceHistory
    --
-- import XMonad.Layout.Accordion
-- import XMonad.Layout.GridVariants (Grid(Grid))
-- import XMonad.Layout.SimplestFloat
-- import XMonad.Layout.SubLayouts
-- import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
-- import XMonad.Layout.Renamed
-- import XMonad.Layout.ShowWName
-- import XMonad.Layout.Simplest
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.VoidBorders
-- import XMonad.Layout.LayoutModifier
-- import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
-- import qualified XMonad.Layout.Magnifier as MG
    --
-- import XMonad.Util.Dmenu


-- mod4Mask= super key
-- mod1Mask= alt key
-- controlMask= ctrl key
-- shiftMask= shift key


myTerminal :: String
myTerminal = "kitty"

myBrowser :: String
myBrowser = "firefox-developer-edition" -- "brave-beta"

myBrowserIncognitoFlag :: String
myBrowserIncognitoFlag = " --private-window"
myEmacs :: String
myEmacs = "emacsclient"

myEditor :: String
myEditor = myTerminal ++ " -e nvim"

myExplorer :: String
myExplorer = "thunar"

myModMask :: KeyMask
myModMask = mod4Mask
myDefaults = def {
        normalBorderColor   = "#4c566a"
      , focusedBorderColor  = "#5e81ac"
      , focusFollowsMouse    = True
      , mouseBindings       = myMouseBindings
      , workspaces          = myWorkspaces
      , keys                = myKeys
      , modMask             = myModMask
      , borderWidth         = 2
      , layoutHook          = myLayoutHook ||| layoutHook myBaseConfig
      , startupHook         = myStartupHook
      , manageHook          = namedScratchpadManageHook myScratchPads <+> manageSpawn <+> myManageHook -- <+> manageHook myBaseConfig
      , handleEventHook     = handleEventHook myBaseConfig -- myEventHook
      , logHook = updatePointer (0.5, 0.5) (0, 0)
      }

-- workspaces
ws1 = "1 \61705"
ws2 = "2 \61729"
ws3 = "3 \61564"
ws4 = "4 \61635"
ws5 = "5 \61502"
ws6 = "6 \62060"
ws7 = "7 \61501"
ws8 = "8 \61564"
ws9 = "9 \62150"
ws10 = "10 \61441"
-- ws1 = "I"
-- ws2 = "II"
-- ws3 = "III"
-- ws4 = "IV"
-- ws5 = "V"
-- ws6 = "VI"
-- ws7 = "VII"
-- ws8 = "VIII"
-- ws9 = "IX"
-- ws10 = "X"
myWorkspaces = [ws1,ws2,ws3,ws4,ws5,ws6,ws7,ws8,ws9,ws10, "NSP"]

-- myWorkspaces    = ["","\61899","\61947","\61635","\61502","\61501","\61705","\61564","\62150","\61872"]
-- myWorkspaces        = ["1 \61705","2 \61729","3 \61564","4 \61635","5 \61502","6 \62060","7 \61501","8 \61564","9 \62150","10 \61441"]
--myWorkspaces    = ["1","2","3","4","5","6","7","8","9","10"]
--myWorkspaces    = ["I","II","III","IV","V","VI","VII","VIII","IX","X"]
-- Icons
-- \61729 


myBaseConfig = desktopConfig

myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"


myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
                , NS "htop" spawnHtop findHtop manageHtop
                -- , NS "notion" spawnNotion findNotion manageNotion
                ]
    where
        spawnTerm   = myTerminal ++ " --title scratchpad"
        findTerm    = title =? "scratchpad"
        manageTerm  = customFloating $ W.RationalRect l t w h
                    where
                        h = 0.9
                        w = 0.9
                        t = 0.95 -h
                        l = 0.95 -w
        spawnCalc   = "galculator"
        findCalc    = className =? "Galculator"
        manageCalc  = customFloating $ W.RationalRect l t w h
                    where
                        h = 0.5
                        w = 0.4
                        t = 0.75 -h
                        l = 0.7 -w
        spawnHtop   = myTerminal ++ " --title scratchpad -e htop"
        findHtop    = title =? "scratchpad"
        manageHtop  = customFloating $ W.RationalRect l t w h
                    where
                        h = 0.9
                        w = 0.9
                        t = 0.95 -h
                        l = 0.95 -w
        spawnNotion = "notion-app" -- ++ ";"
        findNotion  = title =? "notion-app"
        manageNotion = customFloating $ W.RationalRect l t w h
                    where
                        h = 0.9
                        w = 0.9
                        t = 0.95 -h
                        l = 0.95 -w


-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    -- , [className =? "xfce4-notifyd" --> doIgnore]
    , [isFullscreen --> doFullFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [(className =? i <||> resource =? i) --> doIgnore | i <- myIgnores]
    , [className =? x --> hasBorder True | x <- myNoBorders]
    , [(className =? x <||> title =? x <||> resource =? x) --> doFullFloat | x <- myFullFloats]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws1 <+> viewShift ws1 | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws2 <+> viewShift ws2 | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws3 <+> viewShift ws3 | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws4 <+> viewShift ws4 | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws5 <+> viewShift ws5 | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws6 <+> viewShift ws6 | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws7 <+> viewShift ws7 | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws8 <+> viewShift ws8 | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws9 <+> viewShift ws9 | x <- my9Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws10 <+> viewShift ws10 | x <- my10Shifts]
    -- , [(className =? "Spotify") --> doShift ws10 <+> viewShift ws10 | x <- my10Shifts]
    ]
    where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Arandr", "Arcolinux-calamares-tool.py", "Arcolinux-tweak-tool.py", "Arcolinux-welcome-app.py", "Blueberry.py", "Galculator", "feh", "mpv", "Xfce4-terminal", "xdg-desktop-portal-gnome", "Open Files"]
    myTFloats = ["Downloads", "Save As...", "Open Files"]
    myRFloats = []
    myIgnores = ["desktop_window", "xfce4-notifyd"]
    myNoBorders = []
    myFullFloats = ["arcologout.py", "Arcologout.py", "archlinux-logout.py"]
    my1Shifts = []
    my2Shifts = ["emacs", "code"]
    my3Shifts = ["thunar"]
    my4Shifts = []
    my5Shifts = ["Gimp", "feh", "nitrogen"]
    my6Shifts = []
    my7Shifts = ["vlc", "mpv", "Virtualbox"]
    my8Shifts = []
    my9Shifts = ["skype", "discord"]
    my10Shifts = ["spotify", "Spotify", ""]



myLayoutHook = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ gaps [(U, 5), (D, 5), (R, 5), (L, 5)]
            $ avoidStruts
            $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
            $ smartBorders
            $ windowNavigation
            (tiled |||  Grid ||| spiral (6/7) ||| ThreeColMid 1 (3/100) (1/2) ||| noBorders Full)
                where
                tiled   = Tall nmaster delta ratio
                nmaster = 1
                delta   = 3/100
                ratio   = 6/10


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

    ]


-- keys config

-- myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
-- myKeys c =
--     let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
--     subKeys "Xmonad Essentials"
--     [ ("M-C-r", addName "Recompile XMonad"      $ spawn "xmonad --recompile")
--     ]

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS

  [
    ((modMask, xK_b), spawn $ "/home/chachi/.xmonad/scripts/background-select.sh /usr/share/backgrounds/custom/wallpapers")
  , ((modMask, xK_c), spawn $ "code")
  , ((modMask, xK_d), spawn $ "dmenu_run -i -f --nb '#191919' --nf '#fea63c' --sb '#fea63c' --sf '#191919' --font 'FiraCode Nerd Font Mono:bold:pixelsize=14' --render_minheight 30 -p 'Dmenu'")
  -- , ((modMask, xK_e), spawn $ myEmacs ++ " --quiet --create-frame --no-wait --alternate-editor='emacs'" )
  -- , ((modMask, xK_c), spawn $ "conky-toggle" )
  , ((modMask, xK_f), sendMessage $ Toggle NBFULL)
  -- , ((modMask, xK_h), spawn $ "urxvt 'htop task manager' -e htop" )
  -- , ((modMask, xK_m), spawn $ "pragha" )
  , ((modMask, xK_n), spawn $ "feh --randomize --bg-fill --no-xinerama /usr/share/backgrounds/wallpapers/*" )
  , ((modMask, xK_o), spawn $ "xdg-open \"$(fd --type f --glob '*.pdf' /home/chachi/uni | dmenu -i -f --nb '#191919' --nf '#fea63c' --sb '#fea63c' --sf '#191919' --font 'FiraCode Nerd Font Mono:bold:pixelsize=14' --render_minheight 30 -p 'Open')\"" )
  , ((modMask, xK_q), kill )
  -- , ((modMask, xK_r), spawn $ "rofi-theme-selector" )
  -- , ((modMask, xK_t), spawn $ "urxvt" )
  , ((modMask, xK_v), spawn $ "pavucontrol" )
  , ((modMask, xK_w), spawn $ myBrowser)
  , ((modMask, xK_x), spawn $ "archlinux-logout" )
  , ((modMask, xK_y), spawn $ "polybar-msg cmd toggle" )
  , ((modMask, xK_Escape), spawn $ "xkill" )
  , ((modMask, xK_Return), spawn $ myTerminal)
  , ((modMask, xK_KP_Enter), spawn $ myTerminal)
  , ((modMask, xK_space), spawn $ "xkb-switch -n" )
  -- , ((modMask, xK_KP_Return), spawn $ "alacritty" )
  , ((modMask, xK_equal), spawn $ "dmenu_run -i --nb '#191919' --nf '#fea63c' --sb '#fea63c' --sf '#191919' --font 'FiraCode Nerd Font Mono:bold:pixelsize=14' --render_minheight 30 --calc" )
  -- , ((modMask, xK_F1), spawn $ "vivaldi-stable" )
  -- , ((modMask, xK_F2), spawn $ "atom" )
  -- , ((modMask, xK_F3), spawn $ "inkscape" )
  -- , ((modMask, xK_F4), spawn $ "gimp" )
  , ((modMask, xK_F5), spawn $ "meld" )
  , ((modMask, xK_F6), spawn $ "vlc --video-on-top" )
  , ((modMask, xK_F7), spawn $ "virtualbox" )
  , ((modMask, xK_F8), spawn $ myExplorer)
  , ((modMask, xK_F9), spawn $ "evolution" )
  , ((modMask, xK_F10), spawn $ "spotify" )
  -- , ((modMask, xK_F11), spawn $ "rofi -theme-str 'window {width: 100%;height: 100%;}' -show drun" )
  -- , ((modMask, xK_F12), spawn $ "rofi -show drun" )

  -- FUNCTION KEYS
  -- , ((0, xK_F1), namedScratchpadAction myScratchPads "terminal")
  -- , ((0, xK_F12), spawn $ "xfce4-terminal --drop-down" )

  -- SUPER + SHIFT KEYS

  , ((modMask .|. shiftMask , xK_Return ), spawn $ myExplorer)
  , ((modMask .|. shiftMask , xK_KP_Enter ), spawn $ myExplorer)
  , ((modMask .|. shiftMask , xK_d ), spawn $ "dmenu_run -i --nb '#191919' --nf '#fea63c' --sb '#fea63c' --sf '#191919' --font 'FiraCode Nerd Font Mono:bold:pixelsize=14' --render_minheight 30 --calc")
  , ((modMask .|. shiftMask , xK_n ), spawn $ "/home/chachi/.xmonad/scripts/background-select.sh /usr/share/backgrounds/custom/wallpapers")
  , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_q ), kill)
  , ((modMask .|. shiftMask , xK_w ), spawn $ myBrowser ++ " --incognito")
  -- , ((modMask .|. shiftMask , xK_space ), withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1))
  -- , ((modMask .|. shiftMask , xK_x ), io (exitWith ExitSuccess))

  , ((mod1Mask .|. shiftMask , xK_w ), spawn $ myBrowser ++ " --tor")

  -- CONTROL + ALT KEYS

  , ((controlMask .|. mod1Mask , xK_Next ), spawn $ "conky-rotate -n")
  , ((controlMask .|. mod1Mask , xK_Prior ), spawn $ "conky-rotate -p")
  , ((controlMask .|. mod1Mask , xK_a ), spawn $ "xfce4-appfinder")
  , ((controlMask .|. mod1Mask , xK_b ), spawn $ myExplorer)
  -- , ((controlMask .|. mod1Mask , xK_c ), spawn $ "catfish")
  , ((controlMask .|. mod1Mask , xK_e ), spawn $ "archlinux-tweak-tool")
  -- , ((controlMask .|. mod1Mask , xK_f ), spawn $ "firefox")
  -- , ((controlMask .|. mod1Mask , xK_g ), spawn $ "chromium -no-default-browser-check")
  , ((controlMask .|. mod1Mask , xK_i ), spawn $ "nitrogen")
  -- , ((controlMask .|. mod1Mask , xK_k ), spawn $ "arcolinux-logout")
  -- , ((controlMask .|. mod1Mask , xK_l ), spawn $ "arcolinux-logout")
  , ((controlMask .|. mod1Mask , xK_m ), spawn $ "xfce4-settings-manager")
  , ((controlMask .|. mod1Mask , xK_o ), spawn $ "$HOME/.xmonad/scripts/picom-toggle.sh")
  , ((controlMask .|. mod1Mask , xK_p ), spawn $ "pamac-manager")
  -- , ((controlMask .|. mod1Mask , xK_r ), spawn $ "rofi-theme-selector")
  , ((controlMask .|. mod1Mask , xK_s ), spawn $ "spotify")
  , ((controlMask .|. mod1Mask , xK_t ), spawn $ "alacritty")
  , ((controlMask .|. mod1Mask , xK_u ), spawn $ "pavucontrol")
  -- , ((controlMask .|. mod1Mask , xK_v ), spawn $ "vivaldi-stable")
  -- , ((controlMask .|. mod1Mask , xK_w ), spawn $ "arcolinux-welcome-app")
  , ((controlMask .|. mod1Mask , xK_Return ), namedScratchpadAction myScratchPads "terminal")
  , ((controlMask .|. mod1Mask , xK_equal ), namedScratchpadAction myScratchPads "calculator")
  , ((controlMask .|. mod1Mask , xK_h ), namedScratchpadAction myScratchPads "htop")
  , ((controlMask .|. mod1Mask , xK_n ), namedScratchpadAction myScratchPads "notion")

  -- ALT + ... KEYS

  -- , ((mod1Mask, xK_f), spawn $ "variety -f" )
  -- , ((mod1Mask, xK_n), spawn $ "variety -n" )
  -- , ((mod1Mask, xK_p), spawn $ "variety -p" )
  , ((mod1Mask, xK_r), spawn $ "xmonad --restart" )
  -- , ((mod1Mask, xK_t), spawn $ "variety -t" )
  -- , ((mod1Mask, xK_Up), spawn $ "variety --pause" )
  -- , ((mod1Mask, xK_Down), spawn $ "variety --resume" )
  -- , ((mod1Mask, xK_Left), spawn $ "variety -p" )
  -- , ((mod1Mask, xK_Right), spawn $ "variety -n" )
  -- , ((mod1Mask, xK_F2), spawn $ "xfce4-appfinder --collapsed" )
  -- , ((mod1Mask, xK_F3), spawn $ "xfce4-appfinder" )

  --VARIETY KEYS WIT PYWAL

  -- , ((mod1Mask .|. shiftMask , xK_f ), spawn $ "variety -f && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  -- , ((mod1Mask .|. shiftMask , xK_n ), spawn $ "variety -n && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  -- , ((mod1Mask .|. shiftMask , xK_p ), spawn $ "variety -p && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  -- , ((mod1Mask .|. shiftMask , xK_t ), spawn $ "variety -t && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  -- , ((mod1Mask .|. shiftMask , xK_u ), spawn $ "wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")

  --CONTROL + SHIFT KEYS

  , ((controlMask .|. shiftMask , xK_Escape ), spawn $ "xfce4-taskmanager")

  --SCREENSHOTS

  , ((0, xK_Print), spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask, xK_Print), spawn $ "xfce4-screenshooter" )
  , ((controlMask .|. shiftMask , xK_Print ), spawn $ "gnome-screenshot -i")
  , ((controlMask .|. modMask , xK_Print ), spawn $ "flameshot gui")
  , ((shiftMask   .|. modMask , xK_s ), spawn $ "flameshot gui")

  --MULTIMEDIA KEYS

  -- Toggle Touchpad
  , ((0, xF86XK_TouchpadToggle), spawn $ "/home/chachi/.xmonad/scripts/touchpad-toggle.sh")

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn $ "amixer -q set Master toggle")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -q set Master 5%-")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn $ "amixer -q set Master 5%+")

  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 5")

  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 5")

  -- Alternative to increase brightness

  -- Increase brightness
  -- , ((0, xF86XK_MonBrightnessUp),  spawn $ "brightnessctl s 5%+")

  -- Decrease brightness
  -- , ((0, xF86XK_MonBrightnessDown), spawn $ "brightnessctl s 5%-")

--  , ((0, xF86XK_AudioPlay), spawn $ "mpc toggle")
--  , ((0, xF86XK_AudioNext), spawn $ "mpc next")
--  , ((0, xF86XK_AudioPrev), spawn $ "mpc prev")
--  , ((0, xF86XK_AudioStop), spawn $ "mpc stop")

  , ((0, xF86XK_AudioPlay), spawn $ "playerctl play-pause")
  , ((0, xF86XK_AudioNext), spawn $ "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn $ "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn $ "playerctl stop")


  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  -- , ((modMask, xK_space), sendMessage NextLayout)
  , ((modMask, xK_Tab), sendMessage NextLayout)

  , ((modMask .|. shiftMask, xK_Tab), setLayout $ XMonad.layoutHook conf)

  --Focus selected desktop
  , ((mod1Mask, xK_Tab), nextWS)

  --Focus selected desktop
  -- , ((modMask, xK_Tab), nextWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Left ), prevWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Right ), nextWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), withFocused $ windows . W.sink)

  -- Move focus to the next window.
  -- , ((modMask, xK_j), windows W.focusDown)

  -- Move focus to the previous window.
  -- , ((modMask, xK_k), windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
  -- , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused window with the next window.
  -- , ((controlMask .|. modMask, xK_Down), windows W.swapDown  )

  -- Swap the focused window with the previous window.
  -- , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Swap the focused window with the previous window.
  -- , ((controlMask .|. modMask, xK_Up), windows W.swapUp  )

  -- Shrink the master area.
  , ((controlMask .|. modMask .|. mod1Mask , xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((controlMask .|. modMask .|. mod1Mask , xK_l), sendMessage Expand)

  -- Push window back into tiling.
  -- , ((controlMask .|. shiftMask , xK_t), withFocused $ windows . W.sink)

  , ((modMask, xK_l), sendMessage $ Go R)
  , ((modMask, xK_h), sendMessage $ Go L)
  , ((modMask, xK_k), sendMessage $ Go U)
  , ((modMask, xK_j), sendMessage $ Go D)
  , ((modMask .|. shiftMask, xK_l), sendMessage $ Swap R)
  , ((modMask .|. shiftMask, xK_h), sendMessage $ Swap L)
  , ((modMask .|. shiftMask, xK_k), sendMessage $ Swap U)
  , ((modMask .|. shiftMask, xK_j), sendMessage $ Swap D)

  -- Increment the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))

  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)

  --Keyboard layouts
  --qwerty users use this line
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0]

      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
      , (\i -> W.greedyView i . W.shift i, shiftMask)]]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)

  --Keyboard layouts
  --qwerty users use this line
   | (i, k) <- zip (XMonad.workspaces conf) [xK_KP_End, xK_KP_Down, xK_KP_Next, xK_KP_Left, xK_KP_Begin, xK_KP_Right, xK_KP_Home, xK_KP_Up, xK_KP_Prior, xK_KP_Insert]

      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
      , (\i -> W.greedyView i . W.shift i, shiftMask)]]

  ++
  -- ctrl-shift-{w,e,r}, Move client to screen 1, 2, or 3
  -- [((m .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  --    | (key, sc) <- zip [xK_w, xK_e] [0..]
  --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_Left, xK_Right] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

main = do

    -- dbus <- D.connectSession
    -- -- Request access to the DBus name
    -- D.requestName dbus (D.busName_ "org.xmonad.Log")
    --     [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    -- -- xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/scripts/xmobarrc"
    -- -- xmproc1 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/scripts/xmobarrc"
    -- -- xmonad $ ewmhFullscreen $ ewmh $ xmobarProp $
    xmonad $ xmobarProp $ ewmh $ ewmhFullscreen $ myDefaults
     -- . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
        -- {startupHook = myStartupHook
    -- , layoutHook =  myLayout ||| layoutHook myBaseConfig
    -- , manageHook = namedScratchpadManageHook myScratchPads <+> manageSpawn <+> myManageHook -- <+> manageHook myBaseConfig
    -- , modMask = myModMask
    -- , borderWidth = myBorderWidth
    -- , handleEventHook    = handleEventHook myBaseConfig
    -- , focusFollowsMouse = myFocusFollowsMouse
    -- , workspaces = myWorkspaces
    -- , focusedBorderColor = focBorder
    -- , normalBorderColor = normBorder
    -- , keys = myKeys
    -- , mouseBindings = myMouseBindings
    -- , logHook = updatePointer (0.5, 0.5) (0, 0)
    -- , logHook = dynamicLogWithPP $ def {
    --     ppOutput = \x -> System.IO.hPutStrLn xmproc0 x >> System.IO.hPutStrLn xmproc1 x
    --         , ppTitle = xmobarColor myTitleColor "" . ( \ str -> "")
    --         , ppCurrent = xmobarColor myCurrentWSColor "" . wrap """"
    --         , ppVisible = xmobarColor myVisibleWSColor "" . wrap """"
    --         , ppHidden = wrap """"
    --         , ppHiddenNoWindows = xmobarColor myHiddenNoWindowsWSColor ""
    --         , ppUrgent = xmobarColor myUrgentWSColor ""
    --         , ppSep = "  "
    --         , ppWsSep = "  "
    --         , ppLayout = (\ x -> case x of
    --             "Spacing Tall"              -> "<fc=#666666>|</fc>  <fn=1>Tall</fn>"
    --             "Spacing Grid"                 -> "<fc=#666666>|</fc>  <fn=1>Grid</fn>"
    --             "Spacing Spiral"               -> "<fc=#666666>|</fc>  <fn=1>Spiral</fn>"
    --             "Spacing ThreeCol"             -> "<fc=#666666>|</fc>  <fn=1>ThreeColMid</fn>"
    --             "Spacing Full"                 -> "<fc=#666666>|</fc>  <fn=1>Full</fn>"
    --             _                                         -> x )
    --}  updatePointer (0.5, 0.5) (0, 0)
-- }



-- windowCount :: X (Maybe String)
-- windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- myTitleColor = "#c91a1a" -- color of window title
-- myTitleLength = 80 -- truncate window title to this length
-- myCurrentWSColor = "#6790eb" -- color of active workspace
-- myVisibleWSColor = "#aaaaaa" -- color of inactive workspace
-- myUrgentWSColor = "#c91a1a" -- color of workspace with 'urgent' window
-- myHiddenNoWindowsWSColor = "white"

-- myXmobarPP :: PP
-- myXmobarPP = def
--      { ppSep             = magenta " • "
--     , ppTitleSanitize   = xmobarStrip
--     , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
--     , ppHidden          = white . wrap " " ""
--     , ppHiddenNoWindows = lowWhite . wrap " " ""
--     , ppUrgent          = red . wrap (yellow "!") (yellow "!")
--     , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
--     , ppExtras          = [logTitles formatFocused formatUnfocused]
--     }
--   where
--     formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
--     formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

--     -- | Windows should have *some* title, which should not not exceed a
--     -- sane length.
--     ppWindow :: String -> String
--     ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

--     blue, lowWhite, magenta, red, white, yellow :: String -> String
--     magenta  = xmobarColor "#ff79c6" ""
--     blue     = xmobarColor "#bd93f9" ""
--     white    = xmobarColor "#f8f8f2" ""
--     yellow   = xmobarColor "#f1fa8c" ""
--     red      = xmobarColor "#ff5555" ""
--     lowWhite = xmobarColor "#bbbbbb" ""
