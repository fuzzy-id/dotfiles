{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Numeric
import System.Exit
import Test.QuickCheck
import qualified Text.Parsec as P

import Pulse

main :: IO ()
main = runTests >>= flip unless exitFailure

runTests :: IO Bool
runTests = $(quickCheckAll)

instance Arbitrary PulseItem where
  arbitrary = Sink 
              <$> arbitraryString `suchThat` (not . null)  -- sinkName
              <*> arbitrary                                -- sinkDefault
              <*> choose (0,maxBound)                      -- sinkVolume
              <*> arbitrary                                -- sinkMute

arbitraryString :: Gen String
arbitraryString = filter (`notElem` " \n\t\r\f\v\160") <$> arbitrary

class Serializable a where
  serialize :: a -> String

instance Serializable a => Serializable [a] where
  serialize = concatMap serialize

instance Serializable PulseItem where
  serialize Sink{..}
    | sinkDefault = body ++ "set-default-sink " ++ sinkName ++ "\n"
    | otherwise = body
    where body = unlines [serializeVolume, serializeMute]
          serializeVolume = 
            "set-sink-volume " ++ sinkName ++ " 0x" ++ showHex sinkVolume ""
          serializeMute 
            | sinkMute = muteBody ++ " yes"
            | otherwise = muteBody ++ " no"
            where muteBody = "set-sink-mute " ++ sinkName

instance Ord PulseItem where
  compare = compare `on` sinkName

(<&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(f <&> g) x = f x && g x

prop_pDump_on_serialized_eq_id :: PulseItem -> Bool
prop_pDump_on_serialized_eq_id s = [s] == result
  where result = (createSinks . getRight . P.parse pDump m) m
        m = serialize s

prop_pDump_on_serialized_list_eq_id :: [PulseItem] -> Bool
prop_pDump_on_serialized_list_eq_id s = (nubSinks . sort) s == sort result
  where result = (createSinks . getRight . P.parse pDump m) m
        m = serialize s
        nubSinks = nubBy ((==) `on` sinkName)

prop_toggleMute_alters_mute :: PulseItem -> Bool
prop_toggleMute_alters_mute s = sinkMute s /= sinkMute result
  where result = toggleMute s

prop_changeVolumePercent_result_lies_in_boundaries :: Int -> PulseItem -> Bool
prop_changeVolumePercent_result_lies_in_boundaries n =
  ((>= 0) <&> (<= maxVol)). sinkVolume . changeVolumePercent n
