import qualified Data.Map as Map

import XMonad
import qualified XMonad.StackSet as Stack


super = mod1Mask
shift = shiftMask
keys' config = Map.fromList $
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
  ]


terminal' = "xterm"


workspaces' = map show [1 .. 5]


main = xmonad $ def
  { modMask = super
  , workspaces = workspaces'
  , keys = keys'
  }
