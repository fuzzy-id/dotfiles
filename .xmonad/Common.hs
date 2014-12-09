module Common where

reduceToBoundaries :: Ord a => a -> a -> a -> a
reduceToBoundaries min max n 
  | n > max = max
  | n < min = min
  | otherwise = n
