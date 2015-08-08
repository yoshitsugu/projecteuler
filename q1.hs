module Main where

main :: IO ()
main = do
  putStrLn $ show $ sum [x | x <- [3..999], (x `mod` 3) == 0 || (x `mod` 5) == 0]
