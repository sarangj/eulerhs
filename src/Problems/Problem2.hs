module Problems.Problem2 where

import Data.Maybe

nextFib :: (Maybe Int, Maybe Int, Int) -> (Maybe Int, Maybe Int, Int)
nextFib (Nothing, t1, currSum) = (t1, Just 1, currSum)
nextFib (Just t2, Just t1, currSum) = 
  (Just t1, Just (t2 + t1), updateSum currSum (t2+t1))

updateSum :: Int -> Int -> Int
updateSum currSum nextFib = 
  if nextFib `mod` 2 == 0 then currSum + nextFib else currSum

nthFib :: Int -> (Maybe Int, Maybe Int, Int)
nthFib n = foldr f (Nothing, Nothing, 0) [1 .. (n-1)]
  where
    f :: Int -> (Maybe Int, Maybe Int, Int) -> (Maybe Int, Maybe Int, Int)
    f _a (t2May, t1May, currSum) = 
      if fromMaybe True $ (<n+1) <$> t1May
        then nextFib (t2May, t1May, currSum)
        else (t2May, t1May, currSum)

getSolution :: Int
getSolution = evenSum
  where
    (_, _, evenSum) = nthFib 4000000
