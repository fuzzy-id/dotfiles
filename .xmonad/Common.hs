module Common where

addTotalPercentsInBounds :: Integral a => a -> a -> a -> a -> a
addTotalPercentsInBounds totalMin totalMax percent = bounds . (+ change)
  where bounds = reduceToBoundaries totalMin totalMax
        change = (max' * percent) `div` 100
        max' = totalMax - totalMin

reduceToBoundaries :: Ord a => a -> a -> a -> a
reduceToBoundaries lowBound upBound n 
  | n > upBound  = upBound
  | n < lowBound = lowBound
  | otherwise    = n
