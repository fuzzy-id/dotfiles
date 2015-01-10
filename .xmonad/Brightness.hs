module Brightness where

import Control.Applicative
import XMonad

import Common

raiseBrightnessByPercent :: (Functor m,MonadIO m) => Int -> m ()
raiseBrightnessByPercent = changeBrightnessByPercent

lowerBrightnessByPercent :: (Functor m,MonadIO m) => Int -> m ()
lowerBrightnessByPercent n = changeBrightnessByPercent (-n)

changeBrightnessByPercent :: (Functor m,MonadIO m) => Int -> m ()
changeBrightnessByPercent n = 
  getBrightness >>= setBrightness . addTotalPercentsInBounds minBrightness maxBrightness n

getBrightness :: (Functor m, MonadIO m) => m Int
getBrightness = read <$> (io . readFile) brightnessFile

setBrightness :: MonadIO m => Int -> m ()
setBrightness i = 
  spawn $ "sudo -n /bin/sh -c 'echo " ++ show i ++ " > " ++ brightnessFile ++ "'"

brightnessFile :: String
brightnessFile = "/sys/class/backlight/intel_backlight/brightness"

maxBrightness :: Int
maxBrightness = 937

minBrightness :: Int
minBrightness = 0
