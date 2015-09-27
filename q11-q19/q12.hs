module Main where

import Data.List

triangulars = [sum [1..x] | x <- [1..]]

factors :: Int -> [Int]
factors n = _factors n [1..n] []
  where
    _factors :: Int -> [Int] -> [Int] -> [Int]
    _factors _ [] ys = ys
    _factors n (x:xs) ys
      | x^2 > n = ys
      | n `mod` x == 0 = let x' = n `div` x in
          _factors n (filter (/= x') xs) (ys ++ (nub [x,x']))
      | otherwise = _factors n xs ys

main = print $ head $ filter ((> 500) . length . factors) triangulars
