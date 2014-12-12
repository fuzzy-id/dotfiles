module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function
import Data.List
import Pulse
import System.Exit
import Test.HUnit

main :: IO ()
main = do result <- (runTestTT . TestList . map TestCase) tests
          when (errors result + failures result /= 0)
               exitFailure
  where tests = [ test_paSinkMuteToggle
                , test_exactly_one_default_sink
                , test_default_sink_exists
                , test_paSetSink_does_not_alter_sinks
                ]

instance Ord PulseItem where
  compare = compare `on` sinkName

-- Hang around some microseconds till settings propagate
delay = threadDelay 6000


test_paSinkMuteToggle = 
  bracket
    (head <$> paDumpSinks)
    paSetSink
    (\s -> do paSinkMuteToggle s
              delay
              s' <- paGetSinkByName . sinkName $ s
              (not . sinkMute) s' @?= sinkMute s)

test_exactly_one_default_sink = 
  length . filter sinkDefault <$> paDumpSinks >>= (1 @=?)

test_default_sink_exists = do s <- getDefaultSink <$> paDumpSinks
                              sinkDefault s @?= True

test_paSetSink_does_not_alter_sinks = 
  do sinks <- paDumpSinks
     mapM_ paSetSink sinks
     sinks' <- paDumpSinks
     sort sinks @=? sort sinks'

test_paUnmute = bracket (head <$> paDumpSinks) paSetSink test
  where test sink = do when 
                         (not . sinkMute $ sink) 
                         ((paSetSink . toggleMute $ sink) >> delay)
                       sink' <- paGetSinkByName . sinkName $ sink
                       sinkMute sink' @=? True
                       paUnmute sink'
                       delay
                       sink'' <- paGetSinkByName . sinkName $ sink
                       sinkMute sink'' @=? False
