module Main where

import Debug.Trace

type Counter = (Int,Int)

maxStartNumber :: Int -> Int
maxStartNumber n = maxChainNumber [1..n] (1,1)
  where
    maxChainNumber :: [Int] -> Counter -> Int
    maxChainNumber [] (n,_) = n
    maxChainNumber (x:xs) (n,m) = let colt = countCollats x 1 in 
      maxChainNumber xs (if colt > m then (x,colt) else (n,m))

    countCollats :: Int -> Int -> Int
    countCollats x n = if x == 1 then n else countCollats (collats x) (n+1)
     
    collats :: Int -> Int
    collats x = if even x then x `div` 2 else 3 * x + 1

main = print $ maxStartNumber 1000000
