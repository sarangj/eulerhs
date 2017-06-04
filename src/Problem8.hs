module Problem8 where

import Data.Function
import Data.List

import qualified Lib.Misc as Misc

getSolution :: Int
getSolution = product maxList 
  where
    asReversedList = reversedDigits Misc.problem8Num
    subseqs = subsequencesOfLength 13 asReversedList 
    maxList = maximumBy (compare `on` product) subseqs 

subsequencesOfLength :: Int -> [Int] -> [[Int]]
subsequencesOfLength n seq = map f [0 .. ((length seq) - n - 1)]
  where
    f :: Int -> [Int]
    f i = take n $ drop i seq

reversedDigits :: Integer -> [Int]
reversedDigits n
  | n < 10 = [fromIntegral n]
  | otherwise = r : (reversedDigits q)
  where
    r = fromIntegral $ n `mod` 10
    q = n `quot` 10
