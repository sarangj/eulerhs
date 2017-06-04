module Problems.Problem9 where

getSolution :: Int
getSolution = a1 * b1 * (1000 - a1 - b1)
  where
    prs = [ (a, b) | a <- [1 .. 500], b <- [1 .. 500], a < b ] 
    (a1, b1) = head $ filter (matches 1000) prs 


matches :: Int -> (Int, Int) -> Bool
matches n (a, b) = res == 0 
  where
    res = n^2 - 2 * n * a - 2 * n * b + 2 * a * b 
