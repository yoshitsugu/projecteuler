module Main where

import Data.Numbers.Primes

main :: IO ()
main = do
  putStrLn $ show $ primes !! 10000
