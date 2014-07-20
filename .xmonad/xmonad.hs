module Main where

import Codec.Binary.UTF8.String
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

setNeoLayout :: MonadIO m => m ()
setNeoLayout = do spawn "setxkbmap de"
                  dotfiles <- liftM (</> "dotfiles") (io getHomeDirectory)
                  spawn ("xmodmap " ++ dotfiles </> "neo_de.xmodmap")
                  spawn ("xmodmap " ++ dotfiles </> "swap_ctrl_altgr.xmodmap")

data PidProg = PidProg { command :: String
                       , args :: [String]
                       , respawn :: Bool
                       , pidFile :: String
                       }

name :: PidProg -> String
name p = (takeFileName . command) p

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

urxvtd = PidProg "urxvtd" ["-q", "-o"] False ""
redshift = PidProg "gtk-redshift" [] False ""
dropbox dropboxd = PidProg dropboxd ["start"] False ""

main :: IO ()
main = do setNeoLayout
          spawn "xset b off"
          pidPath <- (</> ".xmonad" </> "run") <$> getHomeDirectory
          db <- dropboxExec
          mapM_ startProgWPidFile (map (setPidFile pidPath) [trayer, urxvtd, redshift, db])
          xmonad =<< xmobar myConfig
  where setPidFile path prog = prog {pidFile = path </> (name prog) <.> "pid"}

dropboxExec :: MonadIO m => m PidProg
dropboxExec = liftM 
                (dropbox . (</> ".dropbox-dist" </> "dropboxd")) 
                (io getHomeDirectory)

startProgWPidFile :: PidProg -> IO ()
startProgWPidFile prog = do
  createDirectoryIfMissing True (takeDirectory . pidFile $ prog)
  pidFileExists <- (doesFileExist . pidFile) prog
  if not pidFileExists
     then spawnThis
     else do oldPid <- read <$> (readFile . pidFile) prog
             runs <- checkProgIsRunning oldPid
             if runs
                then when (respawn prog) (do spawn ("kill " ++ show oldPid)
                                             spawnThis)
                else spawnThis
  where spawnThis = spawnWPidFile prog

checkProgIsRunning :: MonadIO m => ProcessID -> m Bool
checkProgIsRunning pid = 
  do result <- io (try $ getProcessPriority pid :: IO (Either IOException Int))
     case result of
       Right _ -> return True
       Left _ -> return False

spawnWPidFile :: MonadIO m => PidProg -> m ()
spawnWPidFile prog = do pid <- spawnPID' prog
                        io (writeFile (pidFile prog) (show pid))
  where spawnPID' p = xfork $ executeFile 
                                (encodeString . command $ p) 
                                True 
                                (map encodeString $ args p)
                                Nothing

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =  
  defaultConfig { terminal = "urxvtc"
                , modMask = mod4Mask
                , normalBorderColor = "black"
                }

