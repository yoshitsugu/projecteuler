module Main where

combination :: Integer -> Integer -> Integer
combination c n = factorial c `div` (factorial (c - n) * (factorial n))
  where
    factorial n = foldl1 (*) [1..n]

main = print $ combination 40 20
