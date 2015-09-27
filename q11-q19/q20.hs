import Data.Char

fact :: Integer -> Integer
fact n = product [1..n]

main = print $ sum $ map digitToInt $ show $ fact 100
