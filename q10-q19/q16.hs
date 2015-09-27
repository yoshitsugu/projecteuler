module Main where

import Data.Char
import Debug.Trace

sumOfDigit :: Integer -> Int
sumOfDigit = sum . map digitToInt . show

main = print $ sumOfDigit $ 2 ^ 1000
