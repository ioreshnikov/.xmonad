import qualified Data.Map
import System.Exit

import XMonad hiding (Font)
import XMonad.Actions.FocusNth
import XMonad.Hooks.DynamicLog hiding (xmobar)
import qualified XMonad.StackSet as Stack


type Font = String
type Foreground = String
type Background = String
type Border = String

data Colors = Colors
  { foreground :: Foreground
  , background :: Background
  , border :: Border
  }
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
  }

twilightDarkTheme = XMonadTheme
  { font = "xft:Ubuntu Mono:size=10"
  , unit = 32
  , normal = Colors "#dcdddd" "#181d23" "#181d23"
  , active = Colors "#00959e" "#1b333e" "#00959e"
  , hidden = Colors "#313c4d" "#181d23" "#181d23"
  , urgent = Colors "#deae3e" "#2a2921" "#deae3e"
  }

theme = twilightDarkTheme


super = mod1Mask
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


terminal' = "xterm"
recompile' = spawn "xmonad --recompile && xmonad --restart"
exit = io exitSuccess


workspaces' = map (:[]) ['α' .. 'ω']


makePrettyPrinter color = def
  { ppCurrent = color (fg . active $ theme) (bg . active $ theme) . dblpad
  , ppHidden = color (fg . normal $ theme) (bg . normal $ theme) . dblpad
  , ppUrgent = color (fg . urgent $ theme) (bg . urgent $ theme) . dblpad
  , ppWsSep = ""
  , ppSep = ""
  , ppTitle = color (fg . normal $ theme) (bg . normal $ theme) . dblpad
  , ppLayout = color (fg . hidden $ theme) (bg . hidden $ theme) . dblpad
  }
  where dblpad = pad . pad

prettyPrinter = makePrettyPrinter xmobarColor
toggleStruts _ = (super, xK_b)
xmobar = statusBar "xmobar" prettyPrinter toggleStruts


config' = def
  { modMask = super
  , workspaces = workspaces'
  , keys = keys'
  , normalBorderColor = bd . normal $ theme
  , focusedBorderColor = bd . active $ theme
  }


main = xmonad =<< xmobar config'
