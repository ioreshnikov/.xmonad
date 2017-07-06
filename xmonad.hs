import XMonad


modMask' = mod1Mask
workspaces' = map show [1 .. 5]


main = xmonad $ def
  { modMask = modMask'
  , workspaces = workspaces'
  }
