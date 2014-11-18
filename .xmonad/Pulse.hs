{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
module Pulse where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import System.Process
import qualified Text.Parsec as P

data PulseItem = Sink { sinkName :: String
                      , sinkDefault :: Bool
                      , sinkVolume :: Int
                      , sinkMute :: Bool
                      } deriving (Show,Eq)

defaultPulseItem :: PulseItem
defaultPulseItem = Sink "" False 0 False

createSinks :: [(String,PulseItem -> PulseItem)] -> [PulseItem]
createSinks = map createSink . groupBySinkName
  where createSink l@((name,_):_) =
          foldr ($) defaultPulseItem ((\s -> s {sinkName = name}):(map snd l))

groupBySinkName :: [(String, b)] -> [[(String, b)]]
groupBySinkName =
  groupBy ((==) `on` fst)
  . sortBy (compare `on` fst)
  . filter ((sinkName defaultPulseItem /=) . fst)

-- Commands

paDump :: IO String
paDump = readProcess "pacmd" ["dump"] ""

-- Parsing
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
          | otherwise = (calc . read . (:[])) c
          where calc n = (n*16^exp + prev,exp+1)
                (prev,exp) = readHex' xs
