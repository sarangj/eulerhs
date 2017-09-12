module Problems.Problem10 where

import Lib.Math

getSolution :: Int 
getSolution = sum $ filter isPrime [1 .. 2000000]
