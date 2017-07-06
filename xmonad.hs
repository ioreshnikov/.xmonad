import qualified XMonad as X


workspaces = map show [1 .. 5]


main = X.xmonad $ X.def
  { X.modMask = X.mod1Mask
  , X.workspaces = workspaces
  }
