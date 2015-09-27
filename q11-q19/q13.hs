module Main where

main = do
  xs <- fmap (map read . lines) getContents
  print . take 10 . show . sum $ xs
