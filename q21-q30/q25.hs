
fib :: [Integer]
fib = 1:1:zipWith (+) fib (tail fib)

countDigit :: Integer -> Int
countDigit = length . show

main = print . snd . head . dropWhile ((< 1000) . countDigit . fst) $ zipWith (,) fib [1..]
