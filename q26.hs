import Data.List (elem)
import Control.Applicative ((<$>),(<*>))

cyclePart :: Int -> [Int]
cyclePart n = _cyclePart 1 n [] []
  where
    _cyclePart :: Int -> Int -> [Int] -> [Int] -> [Int]
    _cyclePart b n xs ys
      | m == 0 = []
      | elem m ys = drop (maximum $ filter ((== m) . (ys !!)) [0..(length ys - 1)]) xs -- おなじ余りがでたら循環する
      | otherwise = _cyclePart m n (xs ++ [q]) (ys ++ [m])
      where
        (q, m) = divMod (b * 10) n

-- 1/d のとき循環値の最大の長さはd - 1
maxCycleLength :: ([Int], Int) -> Bool
maxCycleLength = (==) <$> (length . fst) <*> ((flip (-) 1) . snd)

main = print . snd . head . filter maxCycleLength . map (\x -> (cyclePart x, x)) $ reverse [2..999]
