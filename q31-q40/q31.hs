import Data.List (insert)

coins :: [Int]
coins = [200,100,50,20,10,5,2,1]

comb :: Int -> [Int] -> [[Int]]
comb 0 _ = [[]]
comb n [c] = [[n `div` c]]
comb n (c:cs) = concat [ map (x:) (comb (n - x * c) cs) | x <- [0..n `div` c] ]

main :: IO ()
main = print . length $ comb 200 coins
