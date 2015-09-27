module Main where

import Data.Numbers.Primes

main :: IO ()
main = do
  putStrLn $ show $ sum $ takeWhile (<2000000) primes 
