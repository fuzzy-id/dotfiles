module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Types

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.Script (execScriptHook)

setNeoLayout :: IO ()
setNeoLayout = do spawn "setxkbmap de"
                  dotfiles <- (</> "dotfiles") <$> getHomeDirectory
                  spawn ("xmodmap " ++ dotfiles </> "neo_de.xmodmap")
                  spawn ("xmodmap " ++ dotfiles </> "swap_ctrl_altgr.xmodmap")

data PidProg = PidProg { name :: String
                       , command :: String
                       , respawn :: Bool
                       }

trayer :: PidProg
trayer = PidProg { name = "trayer"
                 , command = intercalate " " ["trayer"
                                             , "--edge top"
                                             , "--align right"
                                             , "--SetDockType true"
                                             , "--SetPartialStrut true"
                                             , "--expand false"
                                             , "--widthtype pixel"
                                             , "--width 120"
                                             , "--heighttype pixel"
                                             , "--height 15"
                                             , "--transparent true"
                                             , "--tint 0x191970"
                                             , "--monitor primary"
                                             ]
                 , respawn = True
                 }

urxvtd = PidProg "urxvtd" "urxvtd -q -o" False
redshift = PidProg "gtk-redshift" "gtk-redshift" False
dropbox dropboxd = PidProg "dropbox" (dropboxd ++ " start") False

main :: IO ()
main = do setNeoLayout
          spawn "xset b off"
          pidPath <- (</> ".xmonad" </> "run") <$> getHomeDirectory
          startProgWPidFile pidPath trayer
          startProgWPidFile pidPath urxvtd
          startProgWPidFile pidPath redshift
          db <- dropbox . (</> ".dropbox-dist" </> "dropboxd") <$> getHomeDirectory
          startProgWPidFile pidPath db
          xmonad =<< xmobar myConfig

startProgWPidFile :: String -> PidProg -> IO ()
startProgWPidFile dir prog = do
  createDirectoryIfMissing True dir
  pidFileExists <- doesFileExist pidFile      
  if not pidFileExists
     then spawnThis
     else do oldPid <- read <$> readFile pidFile
             runs <- checkProgIsRunning oldPid
             if runs
                then when (respawn prog) (do spawn ("kill " ++ show oldPid)
                                             spawnThis)
                else spawnThis
  where pidFile = dir </> (name prog) <.> "pid"
        spawnThis = spawnWPidFile pidFile (command prog)

checkProgIsRunning :: ProcessID -> IO Bool
checkProgIsRunning pid = 
  do result <- try (getProcessPriority pid) :: IO (Either IOException Int)
     case result of
       Right _ -> return True
       Left _ -> return False

spawnWPidFile :: FilePath -> String -> IO ()
spawnWPidFile pidFile exec = do pid <- spawnPID exec
                                writeFile pidFile (show pid) 

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =  
  defaultConfig { terminal = "urxvtc"
                , modMask = mod4Mask
                , normalBorderColor = "black"
                }

