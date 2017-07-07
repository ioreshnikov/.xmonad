{-# LANGUAGE DeriveDataTypeable #-}

-- Imports --
-------------

-- Standard library --
import Data.Data
import qualified Data.Map
import System.Exit

-- Contrib. modules --
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

-- XMonad --
import XMonad hiding (Font)
import XMonad.Actions.FocusNth
import XMonad.Hooks.DynamicLog hiding (xmobar)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
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
  , normal = Colors "#dcdddd" "#181d23" "#181d23"
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
  [ ((super, key), windows $ Stack.greedyView workspace)
  | (workspace, key) <- zip (workspaces config) [xK_F1 .. xK_F9] ]
  ++
  [ ((super .|. shift, key), windows $ Stack.shift workspace)
  | (workspace, key) <- zip (workspaces config) [xK_F1 .. xK_F9] ]
  ++
  [ ((super, key), focusNth index)
  | (index, key) <- zip [0 ..] [xK_1 .. xK_9] ]
  ++
  [ ((super, xK_r), shellPrompt prompt') ]
  ++
  [ ((super, xK_f), fullscreen) ]

-- Workspaces and workscreens --
--------------------------------

workspaces' = map (:[]) ['α' .. 'ω']


-- XMobar --
------------

-- Pretty printer --
makePrettyPrinter color = def
  { ppCurrent = color (fg . active $ theme) (bg . active $ theme) . dblpad
  , ppHidden = color (fg . normal $ theme) (bg . normal $ theme) . dblpad
  , ppUrgent = color (fg . urgent $ theme) (bg . urgent $ theme) . dblpad
  , ppWsSep = ""
  , ppSep = ""
  , ppTitle = const ""
  , ppLayout = color (fg . hidden $ theme) (bg . hidden $ theme) . dblpad
  }
  where dblpad = pad . pad

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

tall = named "T" $ Tall 1 (1/2) (1/2)
mirror = named "M" $ Mirror tall
accordion = named "A" $ Accordion
full = named "F" $ Full
layoutHook' = smartBorders $ tall ||| mirror ||| full ||| accordion


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
  , isFullscreen --> doFullFloat
  ]


-- Config and startup --
------------------------

config' = def
  { modMask = super
  , workspaces = workspaces'
  , keys = keys'
  , layoutHook = layoutHook'
  , manageHook = manageHook'
  , normalBorderColor = bd . normal $ theme
  , focusedBorderColor = bd . active $ theme
  }


main = do
  let templateFile = "/home/me/.xmonad/xmobarrc"
  let outputFile = "/home/me/.xmobarrc"
  compileWithTheme theme templateFile outputFile
  xmonad. withUrgencyHook NoUrgencyHook =<< xmobar config'
