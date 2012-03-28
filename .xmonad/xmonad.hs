import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
     xmproc <- spawnPipe "xmobar ~/.xmobarrc"
     xmonad $ defaultConfig
     	    { manageHook = manageDocks <+> manageHook defaultConfig
	    , layoutHook = avoidStruts $ layoutHook defaultConfig
	    , logHook = dynamicLogWithPP $ xmobarPP
	      	      	{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "green" "" . shorten 50
			}
	    , terminal = "urxvtc"
	    , modMask = mod4Mask
	    , normalBorderColor = "black"
	    }
