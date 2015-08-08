module Main where

fibsFrom1and2 :: [Integer]
fibsFrom1and2 = 1:2:zipWith (+) [x | x <- fibsFrom1and2] [x | x <- tail fibsFrom1and2]


main :: IO ()
main = do
  putStrLn $ show $ sum $ filter even $ takeWhile (<= 4000000) fibsFrom1and2
