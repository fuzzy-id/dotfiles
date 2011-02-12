import XMonad

main = xmonad myConfig

myConfig = defaultConfig {
  terminal = "urxvt",
  modMask = mod4Mask,
  normalBorderColor = "black" }
