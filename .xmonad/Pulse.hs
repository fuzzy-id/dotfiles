{-# LANGUAGE TupleSections #-}
module Pulse where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Numeric (showHex)
import XMonad
import XMonad.Util.Run
import qualified Text.Parsec as P

data PulseItem = Sink { sinkName :: String
                      , sinkDefault :: Bool
                      , sinkVolume :: Int
                      , sinkMute :: Bool
                      } deriving (Show,Eq)

defaultPulseItem :: PulseItem
defaultPulseItem = Sink "" False 0 False

getDefaultSink :: [PulseItem] -> PulseItem
getDefaultSink = head . dropWhile (not . sinkDefault) 

toggleMute :: PulseItem -> PulseItem
toggleMute p = p { sinkMute = (not . sinkMute) p }

changeVolumePercent :: Int -> PulseItem -> PulseItem
changeVolumePercent n p = p {sinkVolume = newVolume}
  where newVolume = reduceToBoundaries 0 maxVol changed
        changed = sinkVolume p + change
        change = (maxVol * n) `div` 100

raiseVolumePercent :: Int -> PulseItem -> PulseItem
raiseVolumePercent = changeVolumePercent

lowerVolumePercent :: Int -> PulseItem -> PulseItem
lowerVolumePercent n = changeVolumePercent (-n)

reduceToBoundaries :: Ord a => a -> a -> a -> a
reduceToBoundaries min max n 
  | n > max = max
  | n < min = min
  | otherwise = n

maxVol :: (Num a, Read a) => a
maxVol = readHex "10000"

-- Commands

paDumpSinks :: (MonadIO m, Functor m) => m [PulseItem]
paDumpSinks = createSinks . getRight . P.parse pDump "pacmd" 
              <$> runProcessWithInput "pacmd" ["dump"] ""

paSinkMuteToggle :: MonadIO m => PulseItem -> m () 
paSinkMuteToggle s = spawn $ "pactl set-sink-mute " ++ sinkName s ++ " toggle"

paRaiseDefaultSinkVolume10Percent :: (MonadIO m, Functor m) => m ()
paRaiseDefaultSinkVolume10Percent = 
  paDumpSinks >>= paSetSinkVolume . raiseVolumePercent 10 . getDefaultSink

paLowerDefaultSinkVolume10Percent :: (MonadIO m, Functor m) => m ()
paLowerDefaultSinkVolume10Percent = 
  paDumpSinks >>= paSetSinkVolume . lowerVolumePercent 10 . getDefaultSink

paSetSinkVolume :: MonadIO m => PulseItem -> m ()
paSetSinkVolume s = spawn $ "pactl " ++ serializeVolume s

serializeVolume :: PulseItem -> String
serializeVolume s = 
  "set-sink-volume " ++ sinkName s ++ " 0x" ++ ((flip showHex "" . sinkVolume) s)

-- Parsing
createSinks :: [(String,PulseItem -> PulseItem)] -> [PulseItem]
createSinks = map createSink . groupBySinkName
  where createSink l@((name,_):_) =
          foldr ($) defaultPulseItem ((\s -> s {sinkName = name}):(map snd l))

groupBySinkName :: [(String, b)] -> [[(String, b)]]
groupBySinkName =
  groupBy ((==) `on` fst)
  . sortBy (compare `on` fst)
  . filter ((sinkName defaultPulseItem /=) . fst)

getRight :: Show a => Either a b -> b
getRight = either (error . show) id

pDump = pSinkRelated `P.sepEndBy` P.string "\n"

pSinkRelated = P.try pSinkMute
               P.<|> P.try pSinkVolume
               P.<|> P.try pSinkDefault
               P.<|> (P.many (P.noneOf "\n") *> pure (sinkName defaultPulseItem,id))

pSinkDefault = (,(\s -> s {sinkDefault = True}))
               <$> (P.string "set-default-sink "
                    *> P.many (P.noneOf "\n"))
pSinkMute = P.string "set-sink-mute "
            *> ((,) <$>  P.anyChar `P.manyTill` P.space)
            <*> ((\m s -> s {sinkMute = m }) <$> pYesOrNo)
pSinkVolume = P.string "set-sink-volume "
              *> ((,) <$>  P.anyChar `P.manyTill` P.string " 0x")
              <*> ((\v s -> s {sinkVolume = v}) . readHex
                   <$> P.many P.alphaNum)
pYesOrNo = (P.string "yes" *> pure True)
           P.<|> (P.string "no" *> pure False)

readHex :: (Num c, Read c) => String -> c
readHex = fst . readHex'
  where readHex' [] = (0,0)
        readHex' (c:xs)
          | c == 'a' = calc 10
          | c == 'b' = calc 11
          | c == 'c' = calc 12
          | c == 'd' = calc 13
          | c == 'e' = calc 14
          | c == 'f' = calc 15
          | otherwise = (calc . read) [c]
          where calc n = (n*16^exp + prev,exp+1)
                (prev,exp) = readHex' xs
