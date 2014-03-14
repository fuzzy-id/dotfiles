import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.Script (execScriptHook)

main :: IO ()
main = xmonad =<< xmobar myConfig

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =  defaultConfig { terminal = "urxvtc"
                          , modMask = mod4Mask
                          , normalBorderColor = "black"
                          , startupHook = execScriptHook ""
                          }


