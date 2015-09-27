module Main where

pythagoreanTriplets :: Integer -> [[Integer]]
pythagoreanTriplets n = [[x, y, (n - x - y)] | x <- [1..n], y <- [x..n], x < y, x^2 + y^2 == (n - x - y)^2]
  
main :: IO ()
main = do
  putStrLn $ show $ product $ head $ pythagoreanTriplets 1000
