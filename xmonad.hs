{-# LANGUAGE DeriveDataTypeable #-}

-- Imports --
-------------

-- Standard library --
import Data.Char
import Data.Data
import Data.List
import qualified Data.Map
import System.Exit

-- Contrib. modules --
import Graphics.X11.ExtraTypes.XF86
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

-- XMonad --
import XMonad hiding (Font)
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.FocusNth
import qualified XMonad.Actions.Workscreen as Workscreen
import XMonad.Hooks.DynamicLog hiding (xmobar)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as Stack
import XMonad.Util.SpawnOnce


-- XMonad theme --
------------------

type Font = String
type Foreground = String
type Background = String
type Border = String

data Colors = Colors
  { foreground :: Foreground
  , background :: Background
  , border :: Border
  } deriving Data

fg = foreground
bg = background
bd = border

data XMonadTheme = XMonadTheme
  { font :: Font
  , unit :: Integer
  , normal :: Colors
  , active :: Colors
  , hidden :: Colors
  , urgent :: Colors
  } deriving Data

arcTheme = XMonadTheme
  { font = "xft:Futura BookC:size=11"
  , unit = 48
  , normal = Colors "#ffffff" "#252a35" "#252a35"
  , active = Colors "#ffffff" "#252a35" "#252a35"
  , hidden = Colors "#aaaaaa" "#252a35" "#252a35"
  , urgent = Colors "#deae3e" "#252a35" "#252a35"
  }

theme = arcTheme


-- Custom commands --
---------------------

terminal' = "xterm"
recompile' = spawn "xmonad --recompile && xmonad --restart"
exit = io exitSuccess

fullscreen = do
    sendMessage ToggleStruts
    sendMessage $ Toggle FULL

tighten = do
  sendMessage ToggleGaps
  sendMessage $ ModifySpacing toggle
    where toggle 0 = quarterunit
          toggle _ = 0

volumeUp = spawn "pamixer -i 10"
volumeDown = spawn "pamixer -d 10"
volumeToggle = spawn "pamixer -t"

brightnessUp = spawn "brightnessctl s 10%+"
brightnessDown = spawn "brightnessctl s 10%-"

lockscreen = spawn "gnome-screensaver-command --lock"

flipScreen = spawn "xrandr --output eDP1 --rotate inverted"
unflipScreen = spawn "xrandr --output eDP1 --rotate normal"


-- Key bindings --
------------------

super = mod4Mask
shift = shiftMask
keys' config = Data.Map.fromList $
  [ ((super, xK_t), spawn $ terminal config)
  , ((super, xK_q), kill)
  , ((super, xK_space), sendMessage NextLayout)
  , ((super, xK_j), windows Stack.focusDown)
  , ((super, xK_k), windows Stack.focusUp)
  , ((super .|. shift, xK_j), windows Stack.swapDown)
  , ((super .|. shift, xK_k), windows Stack.swapUp)
  , ((super, xK_h), sendMessage Shrink)
  , ((super, xK_l), sendMessage Expand)
  , ((super, xK_w), withFocused $ windows . Stack.sink)
  , ((super, xK_comma), sendMessage (IncMasterN 1))
  , ((super, xK_period), sendMessage (IncMasterN (-1)))
  , ((super, xK_Escape), recompile')
  , ((super .|. shift, xK_Escape), exit)
  ]
  ++
  [ ((super, key), Workscreen.viewWorkscreen workscreen)
  | (workscreen, key) <- zip [0 ..] [xK_F1 .. xK_F12] ]
  ++
  [ ((super .|. shift, key), Workscreen.shiftToWorkscreen workscreen)
  | (workscreen, key) <- zip [0 ..] [xK_F1 .. xK_F12] ]
  ++
  [ ((super, xK_Tab), cycleRecentWS [xK_Super_L] xK_Tab xK_Tab) ]
  ++
  [ ((super, key), focusNth index)
  | (index, key) <- zip [0 ..] [xK_1 .. xK_9] ]
  ++
  [ ((super, xK_r), shellPrompt prompt') ]
  ++
  [ ((super, xK_g), tighten),
    ((super, xK_f), fullscreen) ]
  ++
  [ ((0, xF86XK_AudioLowerVolume), volumeDown)
  , ((0, xF86XK_AudioRaiseVolume), volumeUp)
  , ((0, xF86XK_AudioMute), volumeToggle)
  , ((super, xK_Page_Down), volumeDown)
  , ((super, xK_Page_Up), volumeUp)
  , ((super, xK_End), volumeToggle) ]
  ++
  [ ((0, xF86XK_MonBrightnessDown), brightnessDown)
  , ((0, xF86XK_MonBrightnessUp), brightnessUp) ]
  ++
  [ ((super .|. shift, xK_l), lockscreen) ]
  ++
  [ ((super, xK_p), windows copyToAll),
    ((super .|. shift, xK_p), killAllOtherCopies)
  ]
  ++
  [ ((super .|. shift, xK_Left), withFocused (keysResizeWindow (-dx, 0) (0, 0)))
  , ((super .|. shift, xK_Up), withFocused (keysResizeWindow (0, -dy) (0, 0)))
  , ((super .|. shift, xK_Right), withFocused (keysResizeWindow (dx, 0) (0, 0)))
  , ((super .|. shift, xK_Down), withFocused (keysResizeWindow (0, dy) (0, 0)))
  , ((super, xK_Left), withFocused (keysMoveWindow (-dx, 0)))
  , ((super, xK_Up), withFocused (keysMoveWindow (0, -dy)))
  , ((super, xK_Right), withFocused (keysMoveWindow (dx, 0)))
  , ((super, xK_Down), withFocused (keysMoveWindow (0, dy)))
  ]
  ++
  [ ((super, xK_i), flipScreen)
  , ((super .|. shift, xK_i), unflipScreen)
  ]
  where
    dx = fromIntegral . unit $ theme
    dy = dx


-- Workspaces and workscreens --
--------------------------------

numScreens = 1
workspaces' =
  [ "1  <fn=1>\xf14e</fn>  Browser"
  , "2  <fn=1>\xf121</fn>  Emacs"
  , "3  <fn=1>\xf0c3</fn>  Terminal"
  , "4  <fn=1>\xf108</fn>  Workspace"
  , "5  <fn=1>\xf108</fn>  Workspace"
  , "6  <fn=1>\xf1d8</fn>  Telegram"
  , "7  <fn=1>\xf1b2</fn>  VirtualBox"
  ]

fancySubscripts workspace = workspace'
  where
    offset = 0x2080
    underscore = \c -> c == '_'
    name = takeWhile (not . underscore) workspace
    index = tail . dropWhile (not . underscore) $ workspace
    subscript = chr . (+ offset) . read $ index
    workspace' =
        if numScreens == 1
        then name
        else name ++ [subscript]


-- XMobar --
------------

-- Pretty printer --
makePrettyPrinter color = def
  { ppCurrent = color (fg . active $ theme) (bg . active $ theme) . mc
  , ppVisible = color (fg . active $ theme) (bg . active $ theme) . mc
  , ppHidden = color (fg . hidden $ theme) (bg . hidden $ theme) . mc
  , ppHiddenNoWindows = const ""
  , ppUrgent = color (fg . urgent $ theme) (bg . urgent $ theme) . mc
  , ppWsSep = "    "
  , ppSep = "      "
  , ppTitle = const ""
  , ppLayout = color (fg . hidden $ theme) (bg . hidden $ theme) . nl
  }
  where
    mc workspace@(n:_) =
      "<action=xdotool key super+F" ++ [n] ++ ">"
      ++ workspace ++
      "</action>"
    nl layout =
      "<action=xdotool key super+space>"
      ++ layout ++
      "</action>"

prettyPrinter = makePrettyPrinter xmobarColor
toggleStruts = const (super, xK_b)
xmobar = statusBar "xmobar" prettyPrinter toggleStruts

-- Config compilation --
compileWithTheme theme templateFile outputFile = do
    templateString <- readFile templateFile
    let template = newSTMP templateString :: StringTemplate String
    let output = toString $ setAttribute "theme" theme template
    writeFile outputFile output


-- Layouts --
-------------

tabbedConfig = def
  { activeColor = bg . active $ theme
  , activeTextColor = fg . active $ theme
  , activeBorderColor = bg . active $ theme
  , inactiveColor = bg . hidden $ theme
  , inactiveTextColor = fg . hidden $ theme
  , inactiveBorderColor = bg . hidden $ theme
  , urgentColor = bg . urgent $ theme
  , urgentTextColor = fg . urgent $ theme
  , urgentBorderColor = bd . urgent $ theme
  , decoHeight = fromIntegral $ threequarterunit
  , fontName = font $ theme
  }

threequarterunit = fromIntegral . (* 3) $ (unit theme) `div` 4

halfunit = fromIntegral $ (unit theme) `div` 2
quarterunit = fromIntegral $ (unit theme) `div` 4
eighthunit = fromIntegral $ (unit theme) `div` 8

-- halfunit = 0
-- quarterunit = 0

full =
  renamed [Replace "<fn=1>\xf2d0</fn> Fullscreen"]
  $ Full
tall =
  renamed [Replace "<fn=1>\xf0db</fn> Tall"]
  . spacingWithEdge quarterunit
  $ Tall 1 (1/16) (1/2)
grid =
  renamed [Replace "<fn=1>\xf0ce</fn> Grid"]
  . spacingWithEdge quarterunit
  $ Grid
tabbed' =
  renamed [Replace "<fn=2>\xf2d0</fn> Tabbed"]
  . gaps spec
  $ tabbed shrinkText tabbedConfig
    where spec =
            [ (U, halfunit)
            , (R, halfunit)
            , (D, halfunit)
            , (L, halfunit)
            ]

layoutHook' =
  noBorders
  . mkToggle (NOBORDERS ?? FULL ?? EOT)
  $ (tall ||| grid ||| tabbed' ||| full)


-- Prompt --
------------

prompt' = Prompt.def
  { Prompt.font = font theme
  , Prompt.height = fromIntegral . unit $ theme
  , Prompt.fgColor = fg . normal $ theme
  , Prompt.bgColor = bg . normal $ theme
  , Prompt.fgHLight = fg . active $ theme
  , Prompt.bgHLight = bg . active $ theme
  , Prompt.borderColor = bd . normal $ theme
  , Prompt.position = Prompt.Bottom
  }


-- Management --
----------------

manageHook' = composeAll
  [ className =? "gl" --> doFloat
  , className =? "mpv" --> doFloat
  , className =? "Gimp" --> doFloat
  , className =? "Plugin-container" --> doFloat
  , className =? "Gnuplot_qt" --> doFloat
  , className =? "VirtualBox" --> doFloat
  , isFullscreen --> doFullFloat
  ]


-- Urgency --
-------------

withUrgencyHook' =
  withUrgencyHookC
    BorderUrgencyHook { urgencyBorderColor = bd . urgent $ theme }
    urgencyConfig { suppressWhen = Focused }


-- Config and startup --
------------------------

--  Startup --
startupHook' = do
  let workscreenConfig = Workscreen.fromWorkspace numScreens workspaces'
  Workscreen.configWorkscreen workscreenConfig

  spawn "setxkbmap -layout 'us, ru'"
  spawn "setxkbmap -option 'grp:toggle, grp_led:scroll, ctrl:swapcaps'"

  spawn "xinput set-prop 11 275 1"

  spawn "xsetroot -cursor_name left_ptr"
  spawn "bash $HOME/.fehbg"
  spawn "compton -c --no-fading-openclose"

  spawn "xrdb -all $HOME/.Xresources"

  spawn "dropbox"

  return ()

-- Config --
config' = def
  { modMask = super
  , workspaces = workspaces'
  , terminal = terminal'
  , keys = keys'
  , layoutHook = layoutHook'
  , manageHook = manageHook'
  , handleEventHook = handleEventHook def <+> fullscreenEventHook
  , startupHook = startupHook'
  , normalBorderColor = bd . normal $ theme
  , focusedBorderColor = bd . active $ theme
  }


main = do
  let templateFile = "/home/me/.xmonad/xmobarrc"
  let outputFile = "/home/me/.xmobarrc"
  compileWithTheme theme templateFile outputFile

  xmonad . ewmh . withUrgencyHook' =<< xmobar config'
