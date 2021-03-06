------------------------------------
-- Imports
-------------------------------------
import XMonad hiding ( (|||) )              -- Hide ||| for JumpToLayout
import XMonad.Layout hiding ( (|||) )       -- Hide ||| for JumpToLayout
import Data.Monoid
import System.Exit
import Control.Monad                        -- Use "When"
import Data.List                            -- For clickable workspaces

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Actions.Search                -- For search
import XMonad.Actions.Submap
import XMonad.Prompt
import XMonad.Prompt.Input

import XMonad.Actions.Navigation2D          -- Change windows
import XMonad.Actions.GroupNavigation       -- History windows
import XMonad.Actions.NoBorders             -- Toggle borders
import XMonad.Actions.CycleWS               -- cycle thru WS', toggle last WS
import XMonad.Actions.GridSelect            -- Bring selected / Go to selected
import XMonad.Actions.CopyWindow            -- Fixed windows at all workspaces
import XMonad.Actions.Minimize              -- Minimize windows
import XMonad.Actions.FocusNth              -- Focus n-th windows
import XMonad.Hooks.ManageDocks             -- For bars and togglestructs
import XMonad.Hooks.ManageHelpers           -- For different types of float
import XMonad.Hooks.DynamicLog              -- For panel
import XMonad.Hooks.EwmhDesktops            -- Handle full screen
import XMonad.Util.Run                      -- spawnPipe
import XMonad.Util.Dmenu                    -- Dmenu for custom messages
import XMonad.Util.SpawnOnce                -- spawnOnce
import XMonad.Layout.Spacing                -- Gaps and spacing
import XMonad.Layout.MultiToggle            -- Multi Toggle
import XMonad.Layout.MultiToggle.Instances  -- Multi Toggle
import XMonad.Layout.TwoPanePersistent      -- Two pane layout with slave window is persistent
import XMonad.Layout.Grid                   -- Grid layout
import XMonad.Layout.ResizableTile          -- Resizable tall
import XMonad.Layout.ThreeColumns           -- Three columns layout
import XMonad.Layout.NoBorders              -- Smart borders for full screen
import XMonad.Layout.Minimize               -- Layout for minimize
import XMonad.Layout.Tabbed                 -- Layout for tabbed
import XMonad.Layout.LayoutCombinators      -- Jump to layout
import XMonad.Layout.BinarySpacePartition   -- Layout binary
import XMonad.Layout.MultiColumns           -- Layout multicolumns
import XMonad.Layout.Renamed                -- Rename layout
import XMonad.Layout.Reflect                -- Reflect, make master on right
import XMonad.Layout.Magnifier  hiding ( Toggle )             -- Magnifier
import XMonad.Hooks.UrgencyHook             -- Detect urgent window
import XMonad.Hooks.SetWMName               -- Set WM name, may be useful for making Java GUI programs
import XMonad.Hooks.RefocusLast             -- Per workspace focus last

import Local.MC (mouseResizeTile, MCMsg(ResizeCell), mc) -- https://www.reddit.com/r/xmonad/comments/6n1y7d/help_with_layout_one_window_in_masterpane_and/dk6cy0z/


-------------------------------------
-- Modifiers
-------------------------------------
myModMask       = mod4Mask
altMask         = mod1Mask


-------------------------------------
-- Preferred Programs
-------------------------------------
myTerminal      = "xfce4-terminal"
myTray          = "stalonetray"
myDropdownTerm  = "xfce4-terminal --drop-down"
myBrowser       = "qutebrowser"
--myBrowser       = "microsoft-edge-beta"
myMSTeams       = "chromium"
myFileManager   = "lf"
myFont          = "xft:Ubuntu Mono Font:size=10:bold:antialias=true:hinting=true,WenQuanYi Micro Hei:size=10:antialias=true"


-------------------------------------
-- Mouse Action
-------------------------------------
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False


-------------------------------------
-- Windows Decorations
-------------------------------------
myBorderWidth           = 2
myNormalBorderColor     = "#3c3c3c"
myFocusedBorderColor    = "#FF0000"


-------------------------------------
-- Workspaces Setup
-------------------------------------
ws1                     = "\xf57e"
ws2                     = "\xf121"
ws3                     = "\xf1c3"
ws4                     = "\xf07b"
ws5                     = "\xf1ec"
ws6                     = "\xf87c"
ws7                     = "???"
ws8                     = "???"
ws9                     = "\xf03d"
ws10                    = "\xf109"
--myWorkspaces            = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws10]

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]        
--myWorkspaces = clickable . (map xmobarEscape) $ ["1","2","3","4","5","6","7","8","9","10"]
myWorkspaces = clickable . (map xmobarEscape) $ [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws10]
                                                                              
  where                                                                       
         clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..10] l,                                        
                            let n = i ]

-------------------------------------
-- Dmenu Config
-------------------------------------
quitFn :: String -> X ()
quitFn msg = do
    when (msg=="E") (io (exitWith ExitSuccess))
    when (msg=="S") (spawn "systemctl poweroff")
    when (msg=="R") (spawn "systemctl reboot")
    when (msg=="L") (spawn "xscreensaver-command -lock")

quitPrompt :: X ()
quitPrompt = inputPrompt myXPConfig "Exit (E), Shut Down (S), Restart (R), Lock (L)" ?+ quitFn

xrandrFn :: String -> X ()
xrandrFn msg = do
    when (msg=="L") (spawn "xrandr --output eDP-1 --off")
    when (msg=="LO") (spawn "xrandr --output eDP-1 --auto")
    when (msg=="E") (spawn "xrandr --output DP1-3 --auto")
    when (msg=="EO") (spawn "xrandr --output DP1-3 --off")

xrandrPrompt :: X ()
xrandrPrompt = inputPrompt myXPConfig "Laptop Off (L), Laptop On, (LO), External Monitor On (E), External Monitor Off (EO)" ?+ xrandrFn


-------------------------------------
-- XMonad Prompt Config
-------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
    { font                = myFont
    , bgColor             = "#282c34"
    , fgColor             = "#bbc2cf"
    , bgHLight            = "#c792ea"
    , fgHLight            = "#000000"
    , borderColor         = "#535974"
    , promptBorderWidth   = 0
    , position            = Top
    --, position            = CenteredAt { xpCenterY = 0.5, xpWidth = 0.3 }
    , height              = 35
    , historySize         = 0
    , historyFilter       = id
    , defaultText         = []
    , showCompletionOnTab = False
    , alwaysHighlight     = True
    , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
    }

archwiki = searchEngine "Arch Wiki" "https://wiki.archlinux.org/?search="
gscholar = searchEngine "Google Scholar" "https://scholar.google.com/scholar?q="

-------------------------------------
-- Keybindings
-------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,                   xK_Return ), spawn myTerminal)
    , ((modm,                   xK_d      ), spawn "rofi -theme $HOME/Documents/8_Config/1_rofi/themes/menu.rasi -show drun")
    , ((altMask,                xK_i      ), spawn "$HOME/Documents/8_Config/0_scripts/network")
    , ((modm,                   xK_c      ), spawn "zenity --calendar")
    , ((altMask,                xK_q      ), spawn myBrowser)
    , ((modm .|. altMask,       xK_c      ), spawn myMSTeams)
    , ((altMask,                xK_r      ), spawn (myTerminal ++ " -e " ++ myFileManager))
    , ((altMask,                xK_t      ), spawn "thunar")
    , ((altMask,                xK_v      ), spawn (myTerminal ++ " -e nvim"))
    , ((altMask,                xK_c      ), spawn (myDropdownTerm ++ " -e ncmpcpp"))
    , ((0,                      xK_F11    ), spawn (myDropdownTerm ++ " -e python3"))
    , ((0,                      xK_F12    ), spawn myDropdownTerm)

    -- audio control.
    , ((0,                       0x1008FF12 ), spawn "amixer -q set Master toggle")
    , ((0,                       0x1008FF11 ), spawn "amixer -q set Master 5%- unmute")
    , ((0,                       0x1008FF13 ), spawn "amixer -q set Master 5%+ unmute")
    , ((modm .|. controlMask,    xK_Return  ), spawn "amixer -q set Master toggle")
    , ((modm .|. controlMask,    xK_j       ), spawn "amixer -q set Master 5%-")
    , ((modm .|. controlMask,    xK_k       ), spawn "amixer -q set Master 5%+")
    , ((modm .|. controlMask,    xK_Down    ), spawn "amixer -q set Master 5%-")
    , ((modm .|. controlMask,    xK_Up      ), spawn "amixer -q set Master 5%+")
    , ((0,                       0x1008FF17 ), spawn "mpc next")
    , ((0,                       0x1008FF14 ), spawn "mpc toggle")
    , ((0,                       0x1008FF16 ), spawn "mpc prev")
    , ((0,                       0x1008FF15 ), spawn "mpc stop")
    , ((modm,                    xK_period  ), spawn "bash $HOME/Documents/8_Config/0_scripts/sound_output")
    , ((altMask,                 xK_b  ), spawn "bash $HOME/Documents/8_Config/0_scripts/bluetooth")

    -- Screen Brightness and Redshift
    , ((modm .|. shiftMask,      xK_q       ), kill)
    , ((0,                       0x1008FF02 ), spawn "xbacklight -inc 5")
    , ((0,                       0x1008FF03 ), spawn "xbacklight -dec 5")
                                            
    -- Print screen                         
    , ((0,                       xK_Print   ), spawn "scrot $HOME/$(date +%F_%H%M%S_%N).png")
    , ((altMask,                 xK_Print   ), spawn "scrot $HOME/$(date +%F_%H%M%S_%N).png -s")
    --, ((modm .|. shiftMask,      xK_s       ), spawn "$HOME/Documents/8_Config/0_scripts/flameshot_script")
    , ((modm .|. shiftMask,      xK_s       ), spawn "flameshot gui")
                                            
    -- xrandr                               
    , ((modm,                    xK_p       ), xrandrPrompt)
    ]
    ++ -- Windows
    [ ((modm,                    xK_Left    ), windowGo L True)
    , ((modm,                    xK_Right   ), windowGo R True)
    , ((modm,                    xK_Up      ), windowGo U True)
    , ((modm,                    xK_Down    ), windowGo D True)
    , ((modm,                    xK_h       ), windowGo L True)
    , ((modm,                    xK_l       ), windowGo R True)
    , ((modm,                    xK_k       ), windowGo U True)
    , ((modm,                    xK_j       ), windowGo D True)
    , ((altMask .|. shiftMask,   xK_Tab     ), tMaster windows)
    --((modm .|. shiftMask,      xK_m       ), windows W.focusMaster)

    -- Minimizing window
    , ((modm .|. shiftMask,      xK_minus ), withFocused minimizeWindow)
    , ((modm,                    xK_minus ), withLastMinimized maximizeWindowAndFocus)

    -- Window Copying Bindings
    , ((modm .|. controlMask,    xK_a       ), windows copyToAll)
    , ((modm,                    xK_a       ), killAllOtherCopies)
    --, ((modm .|. shiftMask,      xK_a       ), kill1)

    -- Swap adjacent window
    , ((modm .|. shiftMask,      xK_Left    ), windowSwap L True)
    , ((modm .|. shiftMask,      xK_Right   ), windowSwap R True)
    , ((modm .|. shiftMask,      xK_Up      ), windowSwap U True)
    , ((modm .|. shiftMask,      xK_Down    ), windowSwap D True)
    , ((modm .|. shiftMask,      xK_h       ), windowSwap L True)
    , ((modm .|. shiftMask,      xK_l       ), windowSwap R True)
    , ((modm .|. shiftMask,      xK_k       ), windowSwap U True)
    , ((modm .|. shiftMask,      xK_j       ), windowSwap D True)

    , ((modm,                    xK_b       ), goToSelected $ mygridConfig myColorizer) -- go to selected window
    , ((modm .|. controlMask,    xK_b       ), bringSelected $ mygridConfig myColorizer) -- bring selected window
    ]
    ++ -- Layouts
    [ ((modm,                    xK_z       ), sendMessage $ NextLayout)
    , ((modm,                    xK_r       ), sendMessage $ Toggle MIRROR)
    , ((modm,                    xK_w       ), sendMessage $ JumpToLayout "Tabbed")
    , ((modm,                    xK_q       ), sendMessage $ JumpToLayout "ResizableTall")
    , ((modm,                    xK_s       ), sendMessage $ JumpToLayout "3-Col Mid")
    , ((modm,                    xK_g       ), sendMessage $ JumpToLayout "Grid")
    , ((modm,                    xK_e       ), setLayout $ XMonad.layoutHook conf)
    , ((modm,                    xK_f       ), wholeScreen)
    , ((modm .|. shiftMask,      xK_space   ), withFocused toggleFloat)
    , ((modm,                    xK_space   ), switchLayer)
    , ((modm,                    xK_n       ), refresh)
                                            
    , ((modm,                    xK_m       ), nextMatch History (return True))
    , ((modm .|. shiftMask,      xK_m       ), nextMatch History (return True))
    , ((modm,                    xK_m       ), windows W.focusMaster)
    , ((altMask,                 xK_Tab     ), windows W.focusUp)
    , ((altMask .|. shiftMask,   xK_Tab     ), windows W.focusDown)
    --, ((altMask,                 xK_Tab     ), toggleFocus)                        -- Per workspace focus
    --, ((modm,                    xK_m       ), windows W.focusUp)
    --, ((modm .|. shiftMask,      xK_m       ), windows W.focusDown)
    , ((modm .|. shiftMask,      xK_Return  ), tMaster windows)
    , ((altMask .|. shiftMask,   xK_h       ), sendMessage Shrink)                 -- Shrink the master area
    , ((altMask .|. shiftMask,   xK_l       ), sendMessage Expand)                 -- Expand the master area
    , ((altMask .|. shiftMask,   xK_j       ), sendMessage MirrorShrink)           -- Shrink the master area
    , ((altMask .|. shiftMask,   xK_k       ), sendMessage MirrorExpand)           -- Expand the master area
    --, ((modm,                    xK_t       ), withFocused $ windows . W.sink)
    , ((modm,                    xK_Prior   ), sendMessage (IncMasterN 1))         -- Increase number of windows in master
    , ((modm,                    xK_Next  ), sendMessage (IncMasterN (-1)))      -- Decrease number of windows in master

    , ((controlMask .|. shiftMask, xK_j), decWindowSpacing 4)         -- Decrease window spacing
    , ((controlMask .|. shiftMask, xK_k), incWindowSpacing 4)         -- Increase window spacing
    , ((controlMask .|. shiftMask, xK_h), decScreenSpacing 4)         -- Decrease screen spacing
    , ((controlMask .|. shiftMask, xK_l), incScreenSpacing 4)         -- Increase screen spacing
    ]
    ++ -- Search
    [ ((modm,                    xK_o       ), submap . M.fromList $
        [ ((0, xK_a), promptSearchBrowser myXPConfig myBrowser archwiki)
        , ((0, xK_c), promptSearchBrowser myXPConfig myBrowser codesearch)
        , ((0, xK_d), promptSearchBrowser myXPConfig myBrowser duckduckgo)
        , ((0, xK_s), promptSearchBrowser myXPConfig myBrowser google)
        , ((0, xK_g), promptSearchBrowser myXPConfig myBrowser gscholar)
        , ((0, xK_y), promptSearchBrowser myXPConfig myBrowser youtube)
        , ((0, xK_w), promptSearchBrowser myXPConfig myBrowser wikipedia)
        ])
    --, ((modm,                   xK_s        ), selectSearchBrowser myBrowser google)
    , ((modm .|. controlMask,   xK_s        ), promptSearchBrowser myXPConfig myBrowser google)
    ]
    ++ -- Screen
    [ ((m .|. altMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period, xK_slash] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
    ++ -- Workspaces
    [ ((modm,                    xK_1       ), toggleOrView (myWorkspaces !! 0))  -- workspace 1
    , ((modm,                    xK_2       ), toggleOrView (myWorkspaces !! 1))  -- workspace 2
    , ((modm,                    xK_3       ), toggleOrView (myWorkspaces !! 2))  -- workspace 3
    , ((modm,                    xK_4       ), toggleOrView (myWorkspaces !! 3))  -- workspace 4
    , ((modm,                    xK_5       ), toggleOrView (myWorkspaces !! 4))  -- workspace 5
    , ((modm,                    xK_6       ), toggleOrView (myWorkspaces !! 5))  -- workspace 6
    , ((modm,                    xK_7       ), toggleOrView (myWorkspaces !! 6))  -- workspace 7
    , ((modm,                    xK_8       ), toggleOrView (myWorkspaces !! 7))  -- workspace 8
    , ((modm,                    xK_9       ), toggleOrView (myWorkspaces !! 8))  -- workspace 9
    , ((modm,                    xK_0       ), toggleOrView (myWorkspaces !! 9))  -- workspace 10

    -- Move and focus
    , ((modm .|. controlMask,    xK_1       ), (windows $ W.shift (myWorkspaces !! 0)) >> (windows $W.greedyView (myWorkspaces !! 0))) -- shift to WS 1
    , ((modm .|. controlMask,    xK_2       ), (windows $ W.shift (myWorkspaces !! 1)) >> (windows $W.greedyView (myWorkspaces !! 1))) -- shift to WS 2
    , ((modm .|. controlMask,    xK_3       ), (windows $ W.shift (myWorkspaces !! 2)) >> (windows $W.greedyView (myWorkspaces !! 2))) -- shift to WS 3
    , ((modm .|. controlMask,    xK_4       ), (windows $ W.shift (myWorkspaces !! 3)) >> (windows $W.greedyView (myWorkspaces !! 3))) -- shift to WS 4
    , ((modm .|. controlMask,    xK_5       ), (windows $ W.shift (myWorkspaces !! 4)) >> (windows $W.greedyView (myWorkspaces !! 4))) -- shift to WS 5
    , ((modm .|. controlMask,    xK_6       ), (windows $ W.shift (myWorkspaces !! 5)) >> (windows $W.greedyView (myWorkspaces !! 5))) -- shift to WS 6
    , ((modm .|. controlMask,    xK_7       ), (windows $ W.shift (myWorkspaces !! 6)) >> (windows $W.greedyView (myWorkspaces !! 6))) -- shift to WS 7
    , ((modm .|. controlMask,    xK_8       ), (windows $ W.shift (myWorkspaces !! 7)) >> (windows $W.greedyView (myWorkspaces !! 7))) -- shift to WS 8
    , ((modm .|. controlMask,    xK_9       ), (windows $ W.shift (myWorkspaces !! 8)) >> (windows $W.greedyView (myWorkspaces !! 8))) -- shift to WS 9
    , ((modm .|. controlMask,    xK_0       ), (windows $ W.shift (myWorkspaces !! 9)) >> (windows $W.greedyView (myWorkspaces !! 9))) -- shift to WS 10

    -- Move but do not focus
    , ((modm .|. shiftMask,      xK_1       ), (windows $ W.shift (myWorkspaces !! 0))) -- shift to WS 1
    , ((modm .|. shiftMask,      xK_2       ), (windows $ W.shift (myWorkspaces !! 1))) -- shift to WS 2
    , ((modm .|. shiftMask,      xK_3       ), (windows $ W.shift (myWorkspaces !! 2))) -- shift to WS 3
    , ((modm .|. shiftMask,      xK_4       ), (windows $ W.shift (myWorkspaces !! 3))) -- shift to WS 4
    , ((modm .|. shiftMask,      xK_5       ), (windows $ W.shift (myWorkspaces !! 4))) -- shift to WS 5
    , ((modm .|. shiftMask,      xK_6       ), (windows $ W.shift (myWorkspaces !! 5))) -- shift to WS 6
    , ((modm .|. shiftMask,      xK_7       ), (windows $ W.shift (myWorkspaces !! 6))) -- shift to WS 7
    , ((modm .|. shiftMask,      xK_8       ), (windows $ W.shift (myWorkspaces !! 7))) -- shift to WS 8
    , ((modm .|. shiftMask,      xK_9       ), (windows $ W.shift (myWorkspaces !! 8))) -- shift to WS 9
    , ((modm .|. shiftMask,      xK_0       ), (windows $ W.shift (myWorkspaces !! 9))) -- shift to WS 10

    -- Swap workspaces on adjacent screens
    , ((altMask .|. shiftMask,   xK_Right   ), screenSwap R False)
    , ((altMask .|. shiftMask,   xK_Left    ), screenSwap L False)
    , ((altMask .|. shiftMask,   xK_Up      ), screenSwap U False)
    , ((altMask .|. shiftMask,   xK_Down    ), screenSwap D False)

    -- Workspaces navigation
    , ((modm,                    xK_grave   ), toggleWS) -- toggle last workspace (super-tab)
    , ((modm,                    xK_Escape  ), moveTo Prev NonEmptyWS) -- go to prev workspace
    , ((modm,                    xK_Tab     ), moveTo Next NonEmptyWS) -- go to next workspace
    ]
    ++ -- Power
    [ ((modm .|. controlMask,    xK_q       ), quitPrompt)
    , ((modm .|. shiftMask,      xK_r       ), spawn "killall xmobar; xmonad --recompile; xmonad --restart")
    , ((controlMask .|. altMask, xK_Delete  ), spawn "$HOME/Documents/8_Config/3_i3lock/blurlock.sh")

    , ((modm .|. shiftMask,      xK_b       ), sendMessage ToggleStruts)
    ]
    where
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
            then W.sink w s
            else (W.float w (W.RationalRect (1/8) (1/8) (3/4) (3/4)) s))
        wholeScreen = do
            sendMessage $ ToggleStruts 
            sendMessage $ Toggle FULL
        tMaster w = do
            windows W.focusMaster
            toggleFocus
            windows W.swapMaster


-------------------------------------
-- Mouse Bindings
-------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))
    , ((modm, button4), (\w -> moveTo Prev NonEmptyWS))
    , ((modm, button5), (\w -> moveTo Next NonEmptyWS))
    ]


-------------------------------------
-- Layouts
-------------------------------------

-- Define Spacing Raw
mySpacing = spacingRaw True             -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 10 10 10 10) -- Size of screen edge gaps
                       True             -- Enable screen edge gaps
                       (Border 10 10 10 10) -- Size of window gaps
                       True             -- Enable window gaps

mySpacing' = spacingRaw True             -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 20 10 10 10) -- Size of screen edge gaps
                       True             -- Enable screen edge gaps
                       (Border 0 10 10 10) -- Size of window gaps
                       True             -- Enable window gaps

myLayoutHook = avoidStruts
    $ renamed [CutWordsLeft 1]
    $ smartBorders
    $ minimize
    $ mkToggle (NOBORDERS ?? FULL ?? EOT)
    $ mkToggle (single MIRROR)
    $ multicol ||| tiled ||| threecol ||| threecolmid ||| binary ||| grid ||| two_pane ||| tab ||| tab_two
    -- $ minimize (tiled) ||| minimize (Grid) ||| minimize (two_pane) ||| minimize (threecol) ||| minimize (threecolmid)
    where
        tiled       = renamed [Replace "ResizableTall"] $ mySpacing $ reflectVert $ reflectHoriz $magnifiercz' 3 $ ResizableTall nmaster delta ratioT []
        multicol    = renamed [Replace "Tile"] $ mySpacing $ reflectHoriz $ multiCol [1] 1 0.01 (-0.5)
        threecol    = renamed [Replace "3-Col"] $ mySpacing $ reflectVert $ reflectHoriz $ ThreeCol nmaster delta ratio
        threecolmid = renamed [Replace "3-Col Mid"] $ mySpacing $ reflectVert $ ThreeColMid nmaster delta ratio
        binary      = renamed [Replace "BSP"] $ mySpacing $ reflectVert $ emptyBSP
        grid        = renamed [Replace "Grid"] $ mySpacing $ reflectVert $ reflectHoriz $ Grid
        two_pane    = renamed [Replace "Two Pane"] $ mySpacing $ reflectHoriz $ TwoPanePersistent Nothing delta ratio
        tab         = renamed [Replace "Tabbed"] $ reflectHoriz $ tabbed shrinkText myTabConfig
        tab_two     = renamed [Replace "Two Tabbed"] $ mySpacing' $ reflectHoriz $ mc (tabbed shrinkText myTabConfig) [(1, [1]), (1, [1])]

        nmaster = 1 -- The default number of windows in the master pane
        ratioT  = 3/4 -- Default proportion of screen occupied by master pane for Tiled
        ratio   = 1/2 -- Default proportion of screen occupied by master pane
        delta   = 5/100 -- Percent of screen to increment by when resizing panes

myTabConfig = def { activeColor         = "#555555"
                  , activeBorderColor   = "#FFFFFF"
                  , activeBorderWidth   = 0
                  , activeTextColor     = "#FFFFFF"
                  , inactiveColor       = "#2E3440"
                  , inactiveBorderColor = "#555555"
                  , inactiveBorderWidth = 0
                  , inactiveTextColor   = "#7C7C7C"
                  , urgentColor         = "#FDF6E3"
                  , urgentBorderColor   = "#268BD2"
                  , urgentBorderWidth   = 0
                  , urgentTextColor     = "#1ABC9C"
                  , fontName            = "xft:Ubuntu Mono Font:size=10:antialias=true:hinting=true,WenQuanYi Micro Hei:size=10:antialias=true"
                  --, fontName            = "xft:Roboto Mono:size=11:antialias=true"
                  , decoHeight          = 35
                  }

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
    (0x28,0x2c,0x34) -- lowest inactive bg
    (0x28,0x2c,0x34) -- highest inactive bg
    (0xc7,0x92,0xea) -- active bg
    (0xc0,0xa7,0x9a) -- inactive fg
    (0x28,0x2c,0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }


-------------------------------------
-- Windows rule
-------------------------------------
myManageHook = composeAll
    [ appName   =? myBrowser                    --> viewShift (myWorkspaces !! 0)
    , className =? "firefox"                    --> viewShift (myWorkspaces !! 0)
    , className =? "Alacritty"                  --> viewShift (myWorkspaces !! 1)
    , className =? "Xfce4-terminal" <&&> role /=? "xfce4-terminal-dropdown" --> viewShift (myWorkspaces !! 1)
    --, className =? "Xournalpp"                  --> viewShift (myWorkspaces !! 2)
    , title     =? "LibreOffice"                --> viewShift (myWorkspaces !! 2)
    , className =? "Master PDF Editor"          --> viewShift (myWorkspaces !! 2)
    , className =? "Evince"                     --> viewShift (myWorkspaces !! 2)
    , className =? "Wps"                        --> viewShift (myWorkspaces !! 2)
    , className =? "Wpp"                        --> viewShift (myWorkspaces !! 2)
    , className =? "Et"                         --> viewShift (myWorkspaces !! 2)
    , className =? "Thunar"                     --> viewShift (myWorkspaces !! 3)
    , className =? "Pavucontrol"                --> viewShift (myWorkspaces !! 3)
    , className =? "mpv"                        --> viewShift (myWorkspaces !! 3)
    , className =? "MATLAB R2018b"              --> viewShift (myWorkspaces !! 4)
    , className =? "MATLAB R2020a"              --> viewShift (myWorkspaces !! 4)
    , className =? "Gimp-2.10"                  --> viewShift (myWorkspaces !! 5)
    , className =? "Microsoft Teams - Preview"  --> viewShift (myWorkspaces !! 8)
    , className =? "Chromium"                   --> viewShift (myWorkspaces !! 8)
    , className =? "Opera"                      --> viewShift (myWorkspaces !! 8)
    , className =? "Tor Browser"                --> viewShift (myWorkspaces !! 8)
    , className =? "VirtualBox Machine"         --> viewShift (myWorkspaces !! 9)
    , className =? "VirtualBox Manager"         --> viewShift (myWorkspaces !! 9)
    , className =? "Virt-manager"               --> viewShift (myWorkspaces !! 9)

    , role      =? "xfce4-terminal-dropdown"    --> doFloat
    , role      =? "GtkFileChooserDialog"       --> doCenterFloat
    , appName   =? "sun-awt-X11-XFramePeer"     --> doCenterFloat
    , className =? "Xmessage"                   --> doCenterFloat
    , className =? "Zenity"                     --> doFloat
    , role      =? "Dialog"                     --> doCenterFloat
    , role      =? "gimp-toolbox-color-dialog"  --> doCenterFloat
    , title     =? "File Operation Progress"    --> doCenterFloat
    --, appName   =? myTerminal                   --> viewShift (myWorkspaces !! 1)
    --, role      =? "browser-window"             --> doBottomRightFloat
    --, className =? "mpv"                        --> doBottomRightFloat
    ]
    where
        role  = stringProperty "WM_WINDOW_ROLE"
        doBottomRightFloat = doRectFloat $ W.RationalRect (1-mpvw) (1-mpvh) mpvw mpvh
            where
                mpvw = 0.2
                mpvh = 0.3
        viewShift = doF . liftM2 (.) W.greedyView W.shift


------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------
myEventHook = do
    ewmhDesktopsEventHook
    fullscreenEventHook


------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------
barBG       = "#222222"
barFG       = "#FFFFFF"
barLayoutBG = "#9D3E58"
barHint     = "#AA3355"
barCurrent  = "#B79288"
barText     = "#777777"

gray        = "#7F7F7F"
gray2       = "#222222"
red         = "#900000"
blue        = "#2E9AFE"
white       = "#eeeeee"
black       = "#000000"

nordFG      = "#D8DEE9"
nordBG      = "#2E3440"

myLogHook h = do
    historyHook
    dynamicLogWithPP $
        def
        { ppOutput = hPutStrLn h
        , ppCurrent = xmobarColor "#FFFFFF" "" . wrap " " " "
        , ppVisible = xmobarColor "#333333" "" . wrap " " " "
        , ppUrgent = xmobarColor "#FF0000" "" . wrap " " " "
        , ppHidden = xmobarColor "#555555" "" . wrap " " " "
        , ppTitle = xmobarColor "#999999" "" . wrap " " " "
        , ppLayout = xmobarColor "#999999" "" . wrap " " " "
        }

-------------------------------------
-- Windows rule
-------------------------------------
myStartupHook = do
    --spawn "picom --experimental-backends --config $HOME/Documents/8_Config/5_picom/picom.conf"
    spawnOnce "$HOME/Documents/8_Config/0_scripts/monitor_xrandr && xmonad --restart"
    spawnOnce "dunst -config $HOME/Documents/8_Config/4_dunst/dunstrc"
    spawnOnce "xautolock -corners 0-+0 -time 10 -locker '$HOME/Documents/8_Config/3_i3lock/blurlock.sh'"
    spawnOnce "fcitx5"
    spawn "~/.fehbg"

myNavigation2DConfig = def { layoutNavigation = 
    [ ("ResizableTall", sideNavigation)
    , ("Mirror ResizableTall", sideNavigation)
    , ("Grid", sideNavigation)
    , ("3-Col", sideNavigation)
    , ("Mirror 3-Col", sideNavigation)
    , ("3-Col Mid", sideNavigation)
    , ("Mirror 3-Col Mid", sideNavigation)
    , ("BSP", sideNavigation)
    , ("Mirror BSP", sideNavigation)
    , ("Tabbed Simplest", sideNavigation)
    ]}


-------------------------------------
-- Run XMonad
-------------------------------------
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/Documents/8_Config/xmonad/xmobarrc"
    xmonad 
        $ ewmh
        $ docks 
        $ withUrgencyHook NoUrgencyHook
        $ withNavigation2DConfig myNavigation2DConfig
        $ defaults xmproc

defaults xmproc = def {
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
    layoutHook         = refocusLastLayoutHook $ myLayoutHook,
    manageHook         = manageDocks <+> myManageHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook xmproc,
    startupHook        = myStartupHook-- <+> setWMName "LG3D"
}
