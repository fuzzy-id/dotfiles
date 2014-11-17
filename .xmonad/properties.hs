{-# LANGUAGE TemplateHaskell #-}
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
              <$> arbitraryString `suchThat` (not . null)  -- Name
              <*> arbitrary -- defaultSink
              <*> choose (0,maxBound) -- volume
              <*> arbitrary -- mute

arbitraryString = filter (`notElem` " \n\t\r\f\v\160") <$> arbitrary

class Serializable a where
  serialize :: a -> String

instance Serializable a => Serializable [a] where
  serialize = concatMap serialize

instance Serializable PulseItem where
  serialize s@(Sink { sinkDefault = d })
    | d = body ++ "set-default-sink " ++ sinkName s ++ "\n"
    | otherwise = body
    where body = unlines [ serializeVolume s
                         , serializeMute s
                         ]

instance Ord PulseItem where
  compare = compare `on` sinkName

serializeVolume s = 
  "set-sink-volume " ++ sinkName s ++ " 0x" ++ ((flip showHex "" . sinkVolume) s)
serializeMute s =
  "set-sink-mute " ++ sinkName s ++ " " ++ (if sinkMute s then "yes" else "no")

prop_pVolume_on_serialized_gives_id s = (name,volume) == (sinkName s,sinkVolume s)
  where volume = (sinkVolume . sinkMod) defaultPulseItem
        (name,sinkMod) = (getRight . P.parse pSinkVolume m) m
        m = serializeVolume s

prop_pMute_on_serialized_gives_id s = (name,mute) == (sinkName s,sinkMute s)
  where mute = (sinkMute . sinkMod) defaultPulseItem
        (name,sinkMod) =  (getRight . P.parse pSinkMute m) m
        m = serializeMute s

prop_pDump_on_serialized_eq_id s = [s] == result
  where result = (createSinks . getRight . P.parse pDump m) m
        m = serialize s

prop_pDump_on_serialized_list_eq_id s = sort s == sort result
  where result = (createSinks . getRight . P.parse pDump m) m
        m = serialize s
