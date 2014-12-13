{-# LANGUAGE RecordWildCards #-}
module PidProg where

import Codec.Binary.UTF8.String
import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Types
import XMonad

data PidProg = PidProg { command :: String
                       , args :: [String]
                       , respawn :: Bool
                       , pidFile :: String
                       }
                       deriving (Show,Read,Eq)

commandName :: PidProg -> String
commandName = takeFileName . command

makePidProg :: FilePath -> [String] -> Bool -> PidProg
makePidProg command args respawn = PidProg{..}
  where pidFile = ""

readPidFile :: (Read r, MonadIO m) => PidProg -> m r
readPidFile prog = liftM read $ (io . readFile . pidFile) prog

writePidFile :: (Show a, MonadIO m) => PidProg -> a -> m () 
writePidFile prog pid = io $ writeFile (pidFile prog) (show pid)

setPidFile :: FilePath -> PidProg -> PidProg   
setPidFile path prog = 
  prog {pidFile = path </> commandName prog <.> "pid"}

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
    (True, Just pid') -> respawnPidProg prog pid'

initPidPath :: MonadIO m => m FilePath
initPidPath = do pidPath <- liftM (</> ".xmonad" </> "run") $ io getHomeDirectory
                 io $ createDirectoryIfMissing True pidPath
                 return pidPath
