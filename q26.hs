import Data.List

cyclePart :: Int -> [Int]
cyclePart n = _cyclePart 1 n [] []
  where
    _cyclePart :: Int -> Int -> [Int] -> [Int] -> [Int]
    _cyclePart m n xs ys
      | modulo == 0 = []
      | length ys > 0 && elem modulo ys = drop (maximum $ filter ((== modulo) . (ys !!)) [0..(length ys - 1)]) xs -- おなじ余りがでたら循環する
      | otherwise = _cyclePart modulo n (xs ++ [quotient]) (ys ++ [modulo])
      where
        modulo = (m * 10) `mod` n
        quotient = (m * 10) `div` n

main = print . last $ sortBy (\x y -> compare (length $ cyclePart x) (length $ cyclePart y)) [2..999]
