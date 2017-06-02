module Problem1 where

import Data.List

sumWhere :: Num a => (a -> Bool) -> [a] -> a
sumWhere f l = sum (filter f l)

getSolution :: Int
getSolution = sumWhere (\n -> n `mod` 5 == 0 || n `mod` 3 == 0) [0 .. 999]
