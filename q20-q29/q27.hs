import Data.List (elem, maximumBy)
import Data.Ord (comparing)
import Control.Applicative ((<$>), (<*>))

abss :: Integer -> [Integer]
abss x = [negate x..x]

primes :: [Integer]
primes = 1:_primes [2..]
  where
    _primes [] = []
    _primes (n:ns) = n:_primes (filter ((/= 0) . (`mod` n)) ns)

isPrime :: Integer -> Bool
isPrime x = (== x) . head $ dropWhile (< x) primes

consecutivePrimes :: Integer -> Integer -> [Integer]
consecutivePrimes a b = takeWhile isPrime [n^2 + a * n + b | n <- [0..]]

main :: IO ()
main = do
  let ab_pool = abss 999
  print . fst $ maximumBy (comparing snd) [(a * b, length cps) | a <- ab_pool, b <- ab_pool, let cps = consecutivePrimes a b, length cps > 0]
