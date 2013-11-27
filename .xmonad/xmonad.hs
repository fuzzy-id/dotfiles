import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Script (execScriptHook)

main = xmonad =<< xmobar myConfig
myConfig =  defaultConfig
     	    { manageHook = manageDocks <+> manageHook defaultConfig
	    , layoutHook = avoidStruts $ layoutHook defaultConfig
	    , terminal = "urxvtc"
	    , modMask = mod4Mask
	    , normalBorderColor = "black"
	    , startupHook = execScriptHook ""
	    }
