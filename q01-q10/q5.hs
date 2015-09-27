module Main where

main :: IO ()
main = do
  putStrLn $ show $ foldr lcm 20 [1..19]
