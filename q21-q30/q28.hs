diagonals :: Integer -> [Integer]
diagonals 1 = [1]
diagonals n = fmap (\x -> (-) (n ^ 2) ((n - 1) * x)) [0..3]

sumOfDiagonals :: Integer -> Integer
sumOfDiagonals 1 = 1
sumOfDiagonals n = (sum $ diagonals n) + (sumOfDiagonals (n - 2))

main = do
  let n = 1001
  print $ sumOfDiagonals n
