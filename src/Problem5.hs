module Problem5 where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe

getSolution :: Int 
getSolution = HashMap.foldlWithKey' (\t p c -> t * (p ^ c)) 1 m
  where
    m = requiredPrimes [2 .. 20]

requiredPrimes :: [Int] -> HashMap.HashMap Int Int
requiredPrimes l = foldr (HashMap.unionWith max) HashMap.empty  maps 
  where 
    maps = map requiredPrimesImpl l

requiredPrimesImpl :: Int -> HashMap.HashMap Int Int
requiredPrimesImpl n = HashMap.fromListWith (+) [ (p, 1) | p <- primes ]  
  where 
    primes = findPrimeFactors n

findPrimeFactors :: Int -> [Int]
findPrimeFactors n = case firstDivisor n of
  Nothing -> [n]
  Just d -> concat $ map findPrimeFactors [d, n `quot` d]

firstDivisor :: Int -> Maybe Int
firstDivisor n = listToMaybe $ filter f [2 .. maxVal]
  where 
    maxVal = (floor . sqrt . fromIntegral) n
    f :: Int -> Bool
    f d = n `mod` d == 0
