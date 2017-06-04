module Problems.Problem4 where

import Data.List
import Data.Maybe

getSolution :: Int
getSolution = maximum $ filter isPalindrome products
  where 
    products = [ x * y | x <- [100 .. 999], y <- [100 .. 999] ]

isPalindrome :: Int -> Bool
isPalindrome n = n == reverseInt n

reverseInt :: Int -> Int
reverseInt n = (snd . reverseIntImpl) (n, 0)

reverseIntImpl :: (Int, Int) -> (Int, Int)
reverseIntImpl (0, reversed) = (0, reversed)
reverseIntImpl (n, reversed) = reverseIntImpl (q, (reversed * 10) + r) 
  where
    (q, r) = quotientAndRemainder n 10

quotientAndRemainder :: Int -> Int -> (Int, Int)
quotientAndRemainder n d = (n `quot` d, n `mod` d)
