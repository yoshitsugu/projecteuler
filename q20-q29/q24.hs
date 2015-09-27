import Data.List

lexicographicPermutations :: [Int] -> [[Int]]
lexicographicPermutations [] = [[]]
lexicographicPermutations xs = concatMap (\x -> map (x:) $ lexicographicPermutations $ sort [y | y <- xs, y /= x]) xs

main = print . last . take 1000000 $ lexicographicPermutations [0,1,2,3,4,5,6,7,8,9]
