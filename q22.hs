import Data.List.Split (splitOn)
import Data.List (sort, elemIndex)

splitNames :: String -> [String]
splitNames = map strip . splitOn ","
  where
    strip :: String -> String
    strip = tail . init

cal :: [String] -> [Integer]
cal xs = [(fromIntegral (sum $ map alphabeticalPosition x)) * (fromIntegral (i + 1)) | i <- [0..length(xs)-1], let x = xs !! i]

alphabeticalPosition :: Char -> Integer
alphabeticalPosition x = head [n | (c,n) <- zipWith (,) ['A'..'Z'] [1..], c == x]

main = do
  namesTxt <- readFile "q22_names.txt"
  let names = splitNames namesTxt
  print $ sum $ cal $ sort names
