import Data.Set (fromList)
import Data.List (permutations)

pandigitalProducts :: Int -> [Integer]
pandigitalProducts n = map last $ filter isValid $ map (map read) $ split $ concatMap show [1..n]
  where
    split :: String -> [[String]]
    split ns = concatMap (\s -> [[take i s, take j $ drop i s, drop j $ drop i s] | i <- [1..((length s) - 2)], j <- [1..((length s) - i - 1)]]) $ permutations ns

    isValid :: [Integer] -> Bool
    isValid [a,b,c] = a * b == c


main = print . foldl1 (+) . fromList $ pandigitalProducts 9
