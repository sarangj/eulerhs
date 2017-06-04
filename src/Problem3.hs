module Problem3 where

import qualified Lib.Math as Math

getSolution :: Int
getSolution = head $ filter f valsDesc
  where
    num = 600851475143
    maxVal = Math.maxPossiblePrimeFactor num
    valsDesc = [maxVal, maxVal - 1 .. 2]
    f :: Int -> Bool
    f n = num `mod` n == 0 && Math.isPrime n 
