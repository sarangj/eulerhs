module Problems.Problem14 
  ( getSolution 
  ) where 

-- This is stupid slow
getSolution :: Int
getSolution = snd $ maximum $ map (\n -> (findLengthSimple n, n)) [1 .. 1000000] 

findLengthSimple :: Int -> Int
findLengthSimple 1 = 1
findLengthSimple seed = 1 + findLengthSimple (nextCollatz seed) 

nextCollatz :: Int -> Int
nextCollatz n = if n `mod` 2 == 0 then n `div` 2 else (3 * n) + 1
