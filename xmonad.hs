import qualified Data.Map
import System.Exit

import XMonad
import qualified XMonad.Actions.FocusNth as FocusNth
import qualified XMonad.StackSet as Stack


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
  [ ((super, key), windows $ Stack.shift workspace)
  | (workspace, key) <- zip (workspaces config) [xK_F1 .. xK_F9] ]
  ++
  [ ((super, key), FocusNth.focusNth index)
  | (index, key) <- zip [0 ..] [xK_1 .. xK_9] ]


terminal' = "xterm"
recompile' = spawn "xmonad --recompile && xmonad --restart"
exit = io exitSuccess


workspaces' = map show [1 .. 5]


main = xmonad $ def
  { modMask = super
  , workspaces = workspaces'
  , keys = keys'
  }
