module Lib.Math where

maxPossiblePrimeFactor :: Int -> Int
maxPossiblePrimeFactor = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n 
  | n < 2 = False
  | otherwise = not $ any divides [2 .. (maxPossiblePrimeFactor n)]
  where 
    divides :: Int -> Bool
    divides d = n `mod` d == 0 
