module Main where

productsOf3digits :: [Integer]
productsOf3digits = [x * y | x <- threeDigits, y <- threeDigits]
  where
    threeDigits :: [Integer]
    threeDigits = [100..999]

palindrome :: Integer -> Bool
palindrome x = (reverse $ show x) == show x

main :: IO ()
main = do
  putStrLn $ show $ maximum $ filter palindrome productsOf3digits
