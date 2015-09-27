{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Array
import Control.Applicative

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

abundantNumbers :: Integer -> Array Integer Bool
abundantNumbers n = listArray (1, n) $ map isAbundant [1..n]

isAbundant :: Integer -> Bool
isAbundant = (>) <$> (sum . divisors) <*> id

isSumOfAbundants :: Integer -> [Integer] -> Array Integer Bool-> Bool
isSumOfAbundants n (m:ms) as
  | n <= 12 = False
  | (n `div` 2) < m = False
  | as ! (n - m) = True
  | otherwise = isSumOfAbundants n ms as

main = do
  let maxNum = 28124
  let !abMap = abundantNumbers maxNum
  let !abs = filter isAbundant [1..maxNum]
  print . sum $ filter (\x -> not $ isSumOfAbundants x abs abMap) [1..maxNum]
