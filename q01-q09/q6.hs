module Main where

squaresSum :: [Integer] -> Integer
squaresSum = sum . map (^2)

sumsSquare :: [Integer] -> Integer
sumsSquare = (^2) . sum 

main :: IO ()
main = do
  putStrLn $ show $ (sumsSquare [1..100] - squaresSum [1..100])
