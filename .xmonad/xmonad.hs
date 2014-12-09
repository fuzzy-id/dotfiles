module Main where

import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.Map (Map,union,fromList)
import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Types
import System.Process

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.Script (execScriptHook)
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Window

import Brightness
import Pulse

data PidProg = PidProg { command :: String
                       , args :: [String]
                       , respawn :: Bool
                       , pidFile :: String
                       }
                       deriving (Show,Read,Eq)

commandName :: PidProg -> String
commandName = takeFileName . command

makePidProg :: FilePath -> [String] -> Bool -> PidProg
makePidProg c args respawn = PidProg c args respawn ""

readPidFile :: (Read r, MonadIO m) => PidProg -> m r
readPidFile prog = liftM read $ (io . readFile . pidFile) prog

writePidFile :: (Show a, MonadIO m) => PidProg -> a -> m () 
writePidFile prog pid = io $ writeFile (pidFile prog) (show pid)

setPidFile :: FilePath -> PidProg -> PidProg   
setPidFile path prog = 
  prog {pidFile = path </> (commandName prog) <.> "pid"}

pidProgPid :: MonadIO m => PidProg -> m (Maybe ProcessID)
pidProgPid prog = do fileExists <- (io . doesFileExist . pidFile) prog
                     if not fileExists
                        then return Nothing
                        else do oldPid <- readPidFile prog 
                                runs <- doesPidProgRun oldPid
                                if runs
                                   then return (Just oldPid)
                                   else return Nothing

runPidProgs :: MonadIO m => [PidProg] -> m ()
runPidProgs ps = do pidPath <- initPidPath
                    let progs = map (setPidFile pidPath) ps
                    mapM_ runPidProg progs 

spawnPidProg :: MonadIO m => PidProg -> m ()
spawnPidProg prog = io (spawnPID' prog >>= writePidFile prog)
  where spawnPID' p = xfork $ executeFile 
                                (encodeString . command $ p) 
                                True 
                                (map encodeString $ args p)
                                Nothing

respawnPidProg :: (Show a, MonadIO m) => PidProg -> a -> m ()
respawnPidProg prog pid = do spawn ("kill " ++ show pid)
                             spawnPidProg prog

doesPidProgRun :: MonadIO m => ProcessID -> m Bool
doesPidProgRun pid = 
  do result <- io (try $ getProcessPriority pid :: IO (Either IOException Int))
     case result of
       Right _ -> return True
       Left _ -> return False

runPidProg :: MonadIO m => PidProg -> m ()
runPidProg prog = do
  pid <- pidProgPid prog
  case (respawn prog,pid) of
    (_,Nothing) -> spawnPidProg prog
    (False,Just _) -> return ()
    (True, Just pid) -> respawnPidProg prog pid

initPidPath :: MonadIO m => m FilePath
initPidPath = do pidPath <- liftM (</> ".xmonad" </> "run") $ io getHomeDirectory
                 io $ createDirectoryIfMissing True pidPath
                 return pidPath

setNeoLayout :: MonadIO m => m ()
setNeoLayout = do spawn "setxkbmap de"
                  spawn "xmodmap -e 'keycode 166 = Super_R'"
                  dotfiles <- liftM (</> "dotfiles") (io getHomeDirectory)
                  spawn ("xmodmap " ++ dotfiles </> "neo_de.xmodmap")
                  spawn ("xmodmap " ++ dotfiles </> "swap_ctrl_altgr.xmodmap")

urxvtd = makePidProg "urxvtd" ["-q", "-o"] False
redshift = makePidProg "redshift-gtk" [] False
emacsd = makePidProg "emacs" ["--daemon"] False
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

dropboxExec = dropbox . (</> ".dropbox-dist" </> "dropboxd")
              <$> (io getHomeDirectory)
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
                    , ("<XF86AudioLowerVolume>", paLowerDefaultSinkVolume10Percent)
                    , ("<XF86AudioRaiseVolume>", paRaiseDefaultSinkVolume10Percent)
                    , ("<XF86MonBrightnessDown>", changeBrightness (-100))
                    , ("<XF86MonBrightnessUp>", changeBrightness 100)
                    ]

configByHostname :: String -> IO ()
configByHostname s
  | s == "ilxwinb01" = do runPidProgs [nmApplet]
                          return ()
  | otherwise = return ()

nmApplet = makePidProg "nm-applet" [] False

