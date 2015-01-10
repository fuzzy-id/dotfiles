module MyConfig where

import Control.Applicative
import Data.Char
import Data.List
import System.Directory
import System.FilePath

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Shell

import Brightness
import PidProg
import Pulse

myMain :: IO ()
myMain = do setNeoLayout
            spawn "xset b off"
            db <- dropboxExec
            runPidProgs [trayer, urxvtd, redshift, db, emacsd, pidgin]
            -- getHostname >>= configByHostname 
            configByHostname "ilxwinb01"
            xmonad =<< xmobar myConfig

setNeoLayout :: (MonadIO m, Functor m) => m ()
setNeoLayout = spawn $ intercalate 
                         ";" 
                         [ "setxkbmap lv"
                         , "xmodmap " ++ dotfiles </> "neo_de.xmodmap"
                         , "xmodmap -e 'keycode 166 = Super_R'"
                         , "xmodmap " ++ dotfiles </> "swap_ctrl_altgr.xmodmap"
                         ]
  where dotfiles = "$HOME" </> "dotfiles"

urxvtd :: PidProg
urxvtd = makePidProg "urxvtd" ["-q", "-o"] False

redshift :: PidProg
redshift = makePidProg "redshift-gtk" [] False

emacsd :: PidProg
emacsd = makePidProg "emacs" ["--daemon"] False

pidgin :: PidProg
pidgin = makePidProg "pidgin" [] False

trayer :: PidProg
trayer = makePidProg "trayer" 
                     [ "--edge", "top"
                     , "--align", "right"
                     , "--SetDockType", "true"
                     , "--SetPartialStrut", "true"
                     , "--expand", "false"
                     , "--widthtype", "pixel"
                     , "--width", "120"
                     , "--heighttype", "pixel"
                     , "--height", "15"
                     , "--transparent", "true"
                     , "--tint", "0x191970"
                     , "--monitor", "primary"
                     ]
                     True

dropboxExec :: (Functor m,MonadIO m) => m PidProg
dropboxExec = dropbox . (</> ".dropbox-dist" </> "dropboxd")
              <$> io getHomeDirectory

dropbox :: FilePath -> PidProg
dropbox dropboxd = makePidProg dropboxd ["start"] True

getHostname :: (MonadIO m, Functor m) => m String
getHostname = rstrip <$> runProcessWithInput "hostname" [] ""
  where rstrip = reverse . dropWhile isSpace . reverse

myConfig =  
  defaultConfig { terminal = "urxvtc"
                , modMask = mod4Mask
                , normalBorderColor = "black"
                , focusedBorderColor = "#bb0000"
                , layoutHook = smartBorders $ layoutHook defaultConfig ||| tabbed shrinkText defaultTheme ||| Grid
                }
  `additionalKeysP` [ ("<XF86Mail>",spawn "icedove")
                    , ("<XF86HomePage>",spawn "conkeror")
                    , ("<XF86Search>", spawn "emacsclient -nc")
                    , ("<F2>", windowPromptGoto defaultXPConfig)
                    , ("<F3>", shellPrompt defaultXPConfig)
                    , ("<XF86AudioMute>", paToggleDefaultSinkMute)
                    , ("<XF86AudioLowerVolume>", paLowerDefaultSinkVolumeByPercent 5)
                    , ("<XF86AudioRaiseVolume>", paRaiseDefaultSinkVolumeByPercent 5)
                    , ("<XF86MonBrightnessDown>", lowerBrightnessByPercent 5)
                    , ("<XF86MonBrightnessUp>", raiseBrightnessByPercent 5)
                    ]

configByHostname :: (Functor m,MonadIO m) => String -> m ()
configByHostname s
  | s == "ilxwinb01" = const () <$> runPidProgs [nmApplet]
  | otherwise = return ()

nmApplet :: PidProg
nmApplet = makePidProg "nm-applet" [] False

