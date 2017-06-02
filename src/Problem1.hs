module Problem1 where

import Data.List

sumWhere :: Num a => (a -> Bool) -> [a] -> a
sumWhere = sum . filter

getSolution :: Int
getSolution = sumWhere (\n -> n % 5 == 0 || n % 3 == 0) [0 .. 1000]
