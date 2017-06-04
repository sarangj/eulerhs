module Problem6 where

getSolution :: Int
getSolution = getSolutionImpl 100

getSolutionImpl :: Int -> Int
getSolutionImpl n = squareSum - sumSquares 
  where
    sumSquares = sum [ x * x | x <- [1 .. n] ]
    squareSum = (sum [1 .. n]) ^ 2
