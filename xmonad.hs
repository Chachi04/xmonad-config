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
import XMonad.Actions.PhysicalScreens

-- Data
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat, doCenterFloat, isDialog)
import qualified Codec.Binary.UTF8.String as UTF8

-- Layouts
import XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen (fullscreenFull)
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

-- Util
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings, additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Loggers

-- Configs
-- import XMonad.Config.Desktop
-- import XMonad.Config.Azerty

import Graphics.X11.ExtraTypes.XF86
import Control.Monad (liftM2)
import qualified DBus as D
import qualified DBus.Client as D



-- mod4Mask= super key
-- mod1Mask= alt key
-- controlMask= ctrl key
-- shiftMask= shift key


myTerminal :: String
myTerminal = "kitty"

myBrowser :: String
myBrowser = "brave" -- "firefox"

myBrowserIncognitoFlag :: String
myBrowserIncognitoFlag = " --incognito" -- " --private-window"

myEmacs :: String
myEmacs = "emacsclient"

myEditor :: String
myEditor = myTerminal ++ " -e nvim"

myExplorer :: String
myExplorer = myTerminal ++ " --title yazi -e yazi" -- "thunar"


myTaskManager :: String
myTaskManager = myTerminal ++ " --title htop -e htop"

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
      , layoutHook          = myLayoutHook -- ||| layoutHook -- myBaseConfig
      , startupHook         = myStartupHook
      , manageHook          = namedScratchpadManageHook myScratchPads <+> manageSpawn <+> myManageHook -- <+> manageHook myBaseConfig
      -- , handleEventHook     = myHandleEventHook -- handleEventHook myBaseConfig -- myEventHook
      -- , handleEventHook     = ewmhFullscreen <+> docksEventHook
      , logHook = updatePointer (0.5, 0.5) (0, 0) -- or: myLogHook >> updatePointer (0.5, 0.5) (0, 0)
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
myWorkspaces = [ws1,ws2,ws3,ws4,ws5,ws6,ws7,ws8,ws9,ws10] -- "NSP"

-- myWorkspaces    = ["","\61899","\61947","\61635","\61502","\61501","\61705","\61564","\62150","\61872"]
-- myWorkspaces        = ["1 \61705","2 \61729","3 \61564","4 \61635","5 \61502","6 \62060","7 \61501","8 \61564","9 \62150","10 \61441"]
--myWorkspaces    = ["1","2","3","4","5","6","7","8","9","10"]
--myWorkspaces    = ["I","II","III","IV","V","VI","VII","VIII","IX","X"]
-- Icons
-- \61729 

myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"


myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
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
    my10Shifts = ["spotify", "Spotify"]


myLayoutHook = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ gaps [(U, 5), (D, 5), (R, 5), (L, 5)]
            -- $ avoidStruts
            -- $ minimize . BW.boringWindows
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
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS

  [
    -- ((modMask, xK_b), spawn $ "/home/chachi/.xmonad/scripts/background-select.sh /usr/share/backgrounds/wallpapers")
  ((modMask, xK_c), spawn $ "code")
  , ((modMask, xK_d), spawn $ "dmenu_run -i -f --nb '#191919' --nf '#fea63c' --sb '#fea63c' --sf '#191919' --font 'FiraCode Nerd Font Mono:bold:pixelsize=14' --render_minheight 30 -p 'Dmenu'")
  -- , ((modMask, xK_e), spawn $ myEmacs ++ " --quiet --create-frame --no-wait --alternate-editor='emacs'" )
  , ((modMask, xK_f), sendMessage $ Toggle NBFULL)
  , ((modMask, xK_n), spawn $ "~/.xmonad/scripts/random_background.sh") -- --no-xinerama
  , ((modMask, xK_o), spawn $ "xdg-open \"$(fd -I --type f --extension pdf . ~/neorg/uni | dmenu -i -f --nb '#191919' --nf '#fea63c' --sb '#fea63c' --sf '#191919' --font 'FiraCode Nerd Font Mono:bold:pixelsize=14' --render_minheight 30 -p 'Open')\"" )
  , ((modMask, xK_q), kill )
  , ((modMask, xK_v), spawn $ "pavucontrol" )
  , ((modMask, xK_w), spawn $ myBrowser)
  , ((modMask, xK_x), spawn $ "archlinux-logout" )
  , ((modMask, xK_y), spawn $ "polybar-msg cmd toggle" )
  , ((modMask, xK_Escape), spawn $ "xkill" )
  , ((modMask, xK_Return), spawn $ myTerminal)
  , ((modMask, xK_KP_Enter), spawn $ myTerminal)
  -- , ((modMask, xK_space), spawn $ "xkb-switch -n" )
  , ((modMask, xK_equal), spawn $ "dmenu_run -i --nb '#191919' --nf '#fea63c' --sb '#fea63c' --sf '#191919' --font 'FiraCode Nerd Font Mono:bold:pixelsize=14' --render_minheight 30 --calc" )
  -- , ((modMask, xK_F10), spawn $ "spotify" )

  -- FUNCTION KEYS
  -- , ((0, xK_F1), namedScratchpadAction myScratchPads "terminal")
  -- , ((0, xK_F12), spawn $ "xfce4-terminal --drop-down" )

  -- SUPER + SHIFT KEYS

  , ((modMask .|. shiftMask , xK_Return ), spawn $ myExplorer)
  , ((modMask .|. shiftMask , xK_KP_Enter ), spawn $ myExplorer)
  , ((modMask .|. shiftMask , xK_d ), spawn $ "dmenu_run -i --nb '#191919' --nf '#fea63c' --sb '#fea63c' --sf '#191919' --font 'FiraCode Nerd Font Mono:bold:pixelsize=14' --render_minheight 30 --calc")
  , ((modMask .|. shiftMask , xK_n ), spawn $ "feh --bg-fill -z /usr/share/backgrounds/wallpapers/") -- "/home/chachi/.xmonad/scripts/background-select.sh /usr/share/backgrounds/wallpapers")
  , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_q ), kill)
  -- , ((modMask .|. shiftMask , xK_q ), withFocused minimizeWindow)
  -- , ((modMask .|. shiftMask , xK_o ), withLastMinimized maximizeWindow)
  , ((modMask .|. shiftMask , xK_w ), spawn $ myBrowser ++ myBrowserIncognitoFlag)
  -- , ((modMask .|. shiftMask , xK_space ), withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1))
  -- , ((modMask .|. shiftMask , xK_x ), io (exitWith ExitSuccess))

  , ((mod1Mask .|. shiftMask , xK_w ), spawn $ myBrowser ++ " --tor")

  -- CONTROL + ALT KEYS

  -- , ((controlMask .|. mod1Mask , xK_Next ), spawn $ "conky-rotate -n")
  -- , ((controlMask .|. mod1Mask , xK_Prior ), spawn $ "conky-rotate -p")
  -- , ((controlMask .|. mod1Mask , xK_a ), spawn $ "xfce4-appfinder")
  , ((controlMask .|. mod1Mask , xK_b ), spawn $ myExplorer)
  -- , ((controlMask .|. mod1Mask , xK_c ), spawn $ "catfish")
  -- , ((controlMask .|. mod1Mask , xK_e ), spawn $ "archlinux-tweak-tool")
  -- , ((controlMask .|. mod1Mask , xK_f ), spawn $ "firefox")
  -- , ((controlMask .|. mod1Mask , xK_g ), spawn $ "chromium -no-default-browser-check")
  -- , ((controlMask .|. mod1Mask , xK_k ), spawn $ "arcolinux-logout")
  -- , ((controlMask .|. mod1Mask , xK_l ), spawn $ "arcolinux-logout")
  , ((controlMask .|. mod1Mask , xK_m ), spawn $ "xfce4-settings-manager")
  , ((controlMask .|. mod1Mask , xK_o ), spawn $ "$HOME/.xmonad/scripts/picom-toggle.sh")
  , ((controlMask .|. mod1Mask , xK_p ), spawn $ "pamac-manager")
  -- , ((controlMask .|. mod1Mask , xK_r ), spawn $ "rofi-theme-selector")
  , ((controlMask .|. mod1Mask , xK_s ), spawn $ "spotify")
  , ((controlMask .|. mod1Mask , xK_t ), spawn $ myTerminal)
  , ((controlMask .|. mod1Mask , xK_u ), spawn $ "pavucontrol")
  -- , ((controlMask .|. mod1Mask , xK_v ), spawn $ "vivaldi-stable")
  -- , ((controlMask .|. mod1Mask , xK_w ), spawn $ "arcolinux-welcome-app")
  , ((controlMask .|. mod1Mask , xK_Return ), namedScratchpadAction myScratchPads "terminal")
  , ((controlMask .|. mod1Mask , xK_equal ), namedScratchpadAction myScratchPads "calculator")
  , ((controlMask .|. mod1Mask , xK_h ), spawn $ myTaskManager)
  , ((controlMask .|. mod1Mask , xK_n ), namedScratchpadAction myScratchPads "notion")

  -- ALT + ... KEYS

  , ((mod1Mask, xK_r), spawn $ "xmonad --restart" )

  --CONTROL + SHIFT KEYS

  , ((controlMask .|. shiftMask , xK_Escape ), spawn $ myTaskManager)

  --SCREENSHOTS

  -- , ((0, xK_Print), spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask .|. modMask , xK_Print ), spawn $ "flameshot gui")
  , ((shiftMask   .|. modMask , xK_s ), spawn $ "flameshot gui")

  --MULTIMEDIA KEYS

  -- Toggle Touchpad
  , ((0, xF86XK_TouchpadToggle), spawn $ "~/.xmonad/scripts/touchpad-toggle.sh")

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
  -- , ((mod1Mask, xK_Tab), nextWS)

  --Focus selected desktop
  -- , ((modMask, xK_Tab), nextWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Left ), prevWS)
  , ((controlMask .|. mod1Mask , xK_h ), prevWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Right ), nextWS)
  , ((controlMask .|. mod1Mask , xK_l ), nextWS)

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
  -- , ((mod1Mask, xK_l), nextScreen)
  -- , ((mod1Mask, xK_h), prevScreen)
 , ((mod1Mask, xK_h), onPrevNeighbour def W.view)
 , ((mod1Mask, xK_l), onNextNeighbour def W.view)
  -- , ((mod1Mask .|. shiftMask, xK_l), shiftNextScreen)
  -- , ((mod1Mask .|. shiftMask, xK_h), shiftPrevScreen)

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
  -- mod-shift-[1..9] numpad keys, Move client to workspace N
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

 -- ++
 -- --
 -- -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
 -- -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
 -- --
 -- [((modMask .|. mask, key), f sc)
 --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
 --     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]


main = do
    xmonad $ xmobarProp $ ewmhFullscreen $ docks $ ewmh $ myDefaults
