module Main where

intToString :: Int -> String
intToString n
  | n == 1000 = "onethousand"
  | n < 1000 = lessThan1000 n
    where
      lessThan1000 :: Int -> String
      lessThan1000 n
        | n < 100 = lessThan100 n
        | otherwise = let m = n `mod` 100 in
          lessThan100 (n `div` 100) ++ "hundred" ++ (if m == 0 then "" else "and") ++ lessThan100 m

      lessThan20 = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
                    "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

      tensLessThan100 = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

      lessThan100 :: Int -> String
      lessThan100 n
        | n == 0 = ""
        | n < 20 = lessThan20 !! (n - 1)
        | otherwise = let x = n `mod` 10 in
            (tensLessThan100 !! ((n `div` 10) - 2)) ++ (if x == 0 then "" else lessThan20 !! (x - 1))

main = print $ sum $ map (length . intToString) [1..1000]
