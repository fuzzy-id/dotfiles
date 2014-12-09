module Brightness where

import Control.Applicative
import XMonad

import Common

brightnessFile :: String
brightnessFile = "/sys/class/backlight/intel_backlight/brightness"

maxBrightness :: Int
maxBrightness = 937

minBrightness :: Int
minBrightness = 0

getBrightness :: (Functor m, MonadIO m) => m Int
getBrightness = read <$> (io . readFile) brightnessFile

setBrightness :: MonadIO m => Int -> m ()
setBrightness i = 
  spawn $ "sudo -n /bin/sh -c 'echo " ++ show i ++ " > " ++ brightnessFile ++ "'"

changeBrightness :: (Functor m, MonadIO m) => Int -> m ()
changeBrightness i = 
  do old <- getBrightness
     let new = reduceToBoundaries minBrightness maxBrightness (old + i)
     setBrightness new
