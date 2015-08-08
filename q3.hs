module Main where

import Data.Numbers.Primes

main :: IO ()
main = do
  putStrLn $ show $ last $ filter ((== 0) . (mod 600851475143)) $ takeWhile ((< 600851475143) . (^2)) primes
