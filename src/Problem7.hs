module Problem7 where

import qualified Lib.Math as Math

getSolution :: Int
getSolution = nthPrime 10001 

nthPrime :: Int -> Int
nthPrime n
  | n <= 0 = -1
  | otherwise = stepNPrimes 1 n
    
stepNPrimes :: Int -> Int -> Int
stepNPrimes n 0 = n
stepNPrimes n steps = stepNPrimes (nextPrime n) (steps - 1)

nextPrime :: Int -> Int
nextPrime n = head $ filter Math.isPrime [n + 1 .. n + incr] 
  where
    incr = 2 * n
