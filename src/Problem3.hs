module Problem3 where

maxPossiblePrimeFactor :: Int -> Int
maxPossiblePrimeFactor = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime num = not $ any divides [2 .. (maxPossiblePrimeFactor num)]
  where
    divides :: Int -> Bool
    divides d = num `mod` d == 0 

getSolution :: Int
getSolution = head $ filter f valsDesc
  where
    num = 600851475143
    maxVal = maxPossiblePrimeFactor num
    valsDesc = [maxVal, maxVal - 1 .. 2]
    f :: Int -> Bool
    f n = num `mod` n == 0 && isPrime n 
