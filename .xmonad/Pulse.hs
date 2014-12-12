{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Pulse where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Numeric (showHex)
import XMonad
import XMonad.Util.Run
import qualified Text.Parsec as P

import Common

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

maxVol :: (Num a, Read a) => a
maxVol = readHex "10000"

-- Higher Level Commands

paDumpSinks :: (MonadIO m, Functor m) => m [PulseItem]
paDumpSinks = createSinks . getRight . P.parse pDump "pacmd" 
              <$> runProcessWithInput "pacmd" ["dump"] ""

paRaiseDefaultSinkVolumeByPercent :: (MonadIO m, Functor m) => Int -> m ()
paRaiseDefaultSinkVolumeByPercent p = 
  paDumpSinks >>= paSetSinkVolumeAndUnmute . raiseVolumePercent p . getDefaultSink

paLowerDefaultSinkVolumeByPercent :: (MonadIO m, Functor m) => Int -> m ()
paLowerDefaultSinkVolumeByPercent p = 
  paDumpSinks >>= paSetSinkVolume . lowerVolumePercent p . getDefaultSink

paSetSinkVolumeAndUnmute :: MonadIO m => PulseItem -> m ()
paSetSinkVolumeAndUnmute p = do paUnmute p
                                paSetSinkVolume p

paUnmute :: MonadIO m => PulseItem -> m ()
paUnmute s 
  | sinkMute s = paSinkMuteToggle s
  | otherwise = return ()

paGetSinkByName :: (Functor m, MonadIO m) => String -> m PulseItem
paGetSinkByName name = head . filter ((name ==) . sinkName) <$> paDumpSinks

-- Low Level Commands

paSetSink :: MonadIO m => PulseItem -> m ()
paSetSink s = do paSetSinkMute s
                 paSetSinkVolume s
                 paSetSinkDefault s

paSetSinkDefault :: MonadIO m => PulseItem -> m ()
paSetSinkDefault Sink{..} 
  | sinkDefault = spawnPactl ["set-default-sink", sinkName]
  | otherwise = return ()

paSinkMuteToggle :: MonadIO m => PulseItem -> m () 
paSinkMuteToggle Sink{..} = spawnPactl ["set-sink-mute", sinkName, "toggle"]

paSetSinkMute :: MonadIO m => PulseItem -> m ()
paSetSinkMute Sink{..} 
  | sinkMute = spawnPactl ["set-sink-mute", sinkName, "yes"]
  | otherwise = spawnPactl ["set-sink-mute", sinkName, "no"]

paSetSinkVolume :: MonadIO m => PulseItem -> m ()
paSetSinkVolume Sink{..} = 
  spawnPactl ["set-sink-volume", sinkName, "0x" ++ (flip showHex "" sinkVolume)]

spawnPactl :: MonadIO m => [String] -> m ()
spawnPactl = spawn . intercalate " " . ("pactl":)

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

pYesOrNo = pYes P.<|> pNo
pYes = const True <$> P.string "yes"
pNo = const False <$> P.string "no"

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
