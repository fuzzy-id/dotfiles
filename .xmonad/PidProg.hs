{-# LANGUAGE RecordWildCards #-}
module PidProg where

import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Exception
import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Types
import XMonad

makePidProg :: FilePath -> [String] -> Bool -> PidProg
makePidProg command args respawn = PidProg{..}
  where pidFile = ""

data PidProg = PidProg { command :: String
                       , args    :: [String]
                       , respawn :: Bool
                       , pidFile :: String
                       }
                       deriving (Show,Read,Eq)

runPidProgs :: MonadIO m => [PidProg] -> m ()
runPidProgs ps = io $ do pidPath <- initPidPath
                         let progs = map (setPidFile pidPath) ps
                         mapM_ runPidProg progs 

initPidPath :: IO FilePath
initPidPath = do pidPath <- (</> ".xmonad" </> "run") <$> getHomeDirectory
                 createDirectoryIfMissing True pidPath
                 return pidPath

runPidProg :: PidProg -> IO ()
runPidProg prog = do
  pid <- pidProgPid prog
  case (respawn prog,pid) of
    (_,    Nothing  ) -> spawnPidProg prog
    (False,Just _   ) -> return ()
    (True, Just pid') -> respawnPidProg prog pid'

spawnPidProg :: PidProg -> IO ()
spawnPidProg prog = spawnPID' >>= writePidFile prog
  where spawnPID' = xfork $ executeFile 
                              (encodeString . command $ prog)
                              True 
                              (map encodeString $ args prog)
                              Nothing

respawnPidProg :: (Show a) => PidProg -> a -> IO ()
respawnPidProg prog pid = do spawn ("kill " ++ show pid)
                             spawnPidProg prog

readPidFile :: (Read r) => PidProg -> IO r
readPidFile PidProg{..} = read <$> readFile pidFile

writePidFile :: (Show a) => PidProg -> a -> IO () 
writePidFile PidProg{..} = writeFile pidFile . show

setPidFile :: FilePath -> PidProg -> PidProg   
setPidFile path prog = prog {pidFile = path </> commandName <.> "pid"}
  where commandName = (takeFileName . command) prog

pidProgPid :: PidProg -> IO (Maybe ProcessID)
pidProgPid prog = do fileExists <- (doesFileExist . pidFile) prog
                     if not fileExists
                        then return Nothing
                        else do oldPid <- readPidFile prog 
                                runs <- doesPidProgRun oldPid
                                if runs
                                   then return (Just oldPid)
                                   else return Nothing

doesPidProgRun :: ProcessID -> IO Bool
doesPidProgRun pid =
  either (const False) (const True) 
  <$> (try $ getProcessPriority pid :: IO (Either IOException Int))
