module Main where

import Control.Applicative
import Data.Char
import System.Directory
import System.FilePath

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Window

import Brightness
import PidProg
import Pulse

setNeoLayout :: (MonadIO m, Functor m) => m ()
setNeoLayout = do spawn "setxkbmap de"
                  spawn "xmodmap -e 'keycode 166 = Super_R'"
                  dotfiles <- (</> "dotfiles") <$> io getHomeDirectory
                  spawn ("xmodmap " ++ dotfiles </> "neo_de.xmodmap")
                  spawn ("xmodmap " ++ dotfiles </> "swap_ctrl_altgr.xmodmap")

urxvtd :: PidProg
urxvtd = makePidProg "urxvtd" ["-q", "-o"] False

redshift :: PidProg
redshift = makePidProg "redshift-gtk" [] False

emacsd :: PidProg
emacsd = makePidProg "emacs" ["--daemon"] False

trayer :: PidProg
trayer = PidProg { command = "trayer"
                 , args = [ "--edge", "top"
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
                 , respawn = True
                 , pidFile = ""
                 }

dropboxExec :: IO PidProg
dropboxExec = dropbox . (</> ".dropbox-dist" </> "dropboxd")
              <$> (io getHomeDirectory)

dropbox :: FilePath -> PidProg
dropbox dropboxd = makePidProg dropboxd ["start"] True

getHostname :: (MonadIO m, Functor m) => m String
getHostname = rstrip <$> runProcessWithInput "hostname" [] ""
  where rstrip = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do setNeoLayout
          spawn "xset b off"
          db <- dropboxExec
          runPidProgs [trayer, urxvtd, redshift, db, emacsd]
          -- getHostname >>= configByHostname 
          configByHostname "ilxwinb01"
          xmonad =<< xmobar myConfig

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =  
  defaultConfig { terminal = "urxvtc"
                , modMask = mod4Mask
                , normalBorderColor = "black"
                , focusedBorderColor = "#bb0000"
                }
  `additionalKeysP` [ ("<XF86Mail>",spawn "icedove")
                    , ("<XF86HomePage>",spawn "conkeror")
                    , ("<XF86Search>", spawn "emacsclient -nc")
                    , ("<F2>", windowPromptGoto defaultXPConfig)
                    , ("<XF86AudioMute>", paDumpSinks >>= paSinkMuteToggle . getDefaultSink)
                    , ("<XF86AudioLowerVolume>", paLowerDefaultSinkVolumeByPercent 5)
                    , ("<XF86AudioRaiseVolume>", paRaiseDefaultSinkVolumeByPercent 5)
                    , ("<XF86MonBrightnessDown>", changeBrightness (-100))
                    , ("<XF86MonBrightnessUp>", changeBrightness 100)
                    ]

configByHostname :: String -> IO ()
configByHostname s
  | s == "ilxwinb01" = do runPidProgs [nmApplet]
                          return ()
  | otherwise = return ()

nmApplet :: PidProg
nmApplet = makePidProg "nm-applet" [] False

