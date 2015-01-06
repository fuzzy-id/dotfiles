module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function
import Data.List
import System.Exit
import System.Posix.Process
import Test.HUnit

import Pulse
import PidProg

main :: IO ()
main = do result <- (runTestTT . TestList . map TestCase) tests
          when (errors result + failures result /= 0)
               exitFailure
  where tests = [ test_paSinkMuteToggle
                , test_exactly_one_default_sink
                , test_default_sink_exists
                , test_paSetSink_does_not_alter_sinks
                , test_doesPidProgRun_on_own_pid
                , test_doesPidProgRun_on_non_existent_pid
                ]

instance Ord PulseItem where
  compare = compare `on` sinkName

-- Hang around some microseconds till settings propagate
delay :: IO ()
delay = threadDelay 6000

test_paSinkMuteToggle :: Assertion
test_paSinkMuteToggle = 
  bracket
    (head <$> paDumpSinks)
    paSetSink
    (\s -> do paSinkMuteToggle s
              delay
              s' <- paGetSinkByName . sinkName $ s
              (not . sinkMute) s' @?= sinkMute s)

test_exactly_one_default_sink :: Assertion
test_exactly_one_default_sink = 
  length . filter sinkDefault <$> paDumpSinks >>= (1 @=?)

test_default_sink_exists :: Assertion
test_default_sink_exists = do s <- getDefaultSink <$> paDumpSinks
                              sinkDefault s @?= True

test_paSetSink_does_not_alter_sinks :: Assertion
test_paSetSink_does_not_alter_sinks = 
  do sinks <- paDumpSinks
     mapM_ paSetSink sinks
     sinks' <- paDumpSinks
     sort sinks @=? sort sinks'

test_paUnmute :: Assertion
test_paUnmute = bracket (head <$> paDumpSinks) paSetSink t
  where t sink = do unless 
                      (sinkMute sink)
                      (paSetSink . toggleMute $ sink) >> delay
                    sink' <- paGetSinkByName . sinkName $ sink
                    sinkMute sink' @=? True
                    paUnmute sink'
                    delay
                    sink'' <- paGetSinkByName . sinkName $ sink
                    sinkMute sink'' @=? False

test_doesPidProgRun_on_own_pid :: Assertion
test_doesPidProgRun_on_own_pid = getProcessID >>= doesPidProgRun >>= (@=? True)

test_doesPidProgRun_on_non_existent_pid :: Assertion
test_doesPidProgRun_on_non_existent_pid = doesPidProgRun (-1) >>= (@=? False)
