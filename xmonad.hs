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
import XMonad.Actions.FocusNth
import qualified XMonad.Actions.Workscreen as Workscreen
import XMonad.Hooks.DynamicLog hiding (xmobar)
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

twilightDarkTheme = XMonadTheme
  { font = "xft:Ubuntu Mono:size=10"
  , unit = 32
  , normal = Colors "#dcdddd" "#181d23" "#313c4d"
  , active = Colors "#00959e" "#1b333e" "#00959e"
  , hidden = Colors "#313c4d" "#181d23" "#181d23"
  , urgent = Colors "#deae3e" "#2a2921" "#deae3e"
  }

theme = twilightDarkTheme


-- Custom commands --
---------------------

terminal' = "xterm"
recompile' = spawn "xmonad --recompile && xmonad --restart"
exit = io exitSuccess

fullscreen = do
    sendMessage ToggleStruts
    sendMessage (Toggle FULL)

volumeUp = spawn "amixer -M -D pulse set Master 5%+"
volumeDown = spawn "amixer -M -D pulse set Master 5%-"
volumeToggle = spawn "amixer -D pulse set Master toggle"

brightnessUp = spawn "xbacklight + 10"
brightnessDown = spawn "xbacklight - 10"

lockscreen = spawn "gnome-screensaver-command --lock"


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
  [ ((super, key), focusNth index)
  | (index, key) <- zip [0 ..] [xK_1 .. xK_9] ]
  ++
  [ ((super, xK_r), shellPrompt prompt') ]
  ++
  [ ((super, xK_f), fullscreen) ]
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
  [ ((super .|. shift, xK_l), lockscreen)]


-- Workspaces and workscreens --
--------------------------------

numScreens = 2
workspaces' =
  Workscreen.expandWorkspace numScreens
  [ "ω", "ξ", "ε", "τ"
  , "α", "β", "γ", "δ"
  , "ζ", "η", "θ", "ι"
  ]

fancySubscripts workspace = workspace'
  where
    offset = 0x2080
    underscore = \c -> c == '_'
    name = takeWhile (not . underscore) workspace
    index = tail . dropWhile (not . underscore) $ workspace
    subscript = chr . (+ offset) . read $ index
    workspace' = name ++ [subscript]


-- XMobar --
------------

-- Pretty printer --
makePrettyPrinter color = def
  { ppCurrent = color (bg . active $ theme) (fg . active $ theme) . un
  , ppVisible = color (fg . active $ theme) (bg . active $ theme) . un
  , ppHidden = color (fg . normal $ theme) (bg . normal $ theme) . un
  , ppUrgent = color (fg . urgent $ theme) (bg . urgent $ theme) . un
  , ppWsSep = ""
  , ppSep = "  "
  , ppTitle = const ""
  , ppLayout = color (fg . hidden $ theme) (bg . hidden $ theme)
  }
  where
    un = pad . fancySubscripts

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
  , inactiveColor = bg . normal $ theme
  , inactiveTextColor = fg . normal $ theme
  , inactiveBorderColor = bg . normal $ theme
  , urgentColor = bg . urgent $ theme
  , urgentTextColor = fg . urgent $ theme
  , urgentBorderColor = bd . urgent $ theme
  , decoHeight = fromIntegral . unit $ theme
  , fontName = font $ theme
  }

halfunit = fromIntegral $ (unit theme) `div` 2
quarterunit = fromIntegral $ (unit theme) `div` 4

full =
  renamed [Replace "Full"]
  $ Full
tall =
  renamed [Replace "Tall"]
  . spacingWithEdge quarterunit
  $ Tall 1 (1/16) (1/2)
grid =
  renamed [Replace "Grid"]
  . spacingWithEdge quarterunit
  $ Grid
tabbed' =
  renamed [Replace "Tbbd"]
  . gaps spec
  $ tabbed shrinkText tabbedConfig
    where spec = [(U, halfunit), (R, halfunit), (D, halfunit), (L, halfunit)]

layoutHook' = smartBorders $ tall ||| grid ||| tabbed' ||| full


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
  , Prompt.position = Prompt.Top
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
  return ()


-- Config --
config' = def
  { modMask = super
  , workspaces = workspaces'
  , keys = keys'
  , layoutHook = layoutHook'
  , manageHook = manageHook'
  , startupHook = startupHook'
  , normalBorderColor = bd . normal $ theme
  , focusedBorderColor = bd . active $ theme
  }


main = do
  let templateFile = "/home/me/.xmonad/xmobarrc"
  let outputFile = "/home/me/.xmobarrc"
  compileWithTheme theme templateFile outputFile
  xmonad . withUrgencyHook' =<< xmobar config'
