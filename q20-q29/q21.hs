
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

amicableNumbers :: [Integer]
amicableNumbers = [x | x <- [1..], amicable x]
  where
    amicable :: Integer -> Bool
    amicable x = let pair = sum $ divisors x in
      (pair /= x) && (sum $ divisors pair) == x

main = print $ sum $ takeWhile (< 10000) amicableNumbers
