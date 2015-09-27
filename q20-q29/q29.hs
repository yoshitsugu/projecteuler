import Data.Set (fromList, size)

main = print . size $ fromList [a ^ b | a <- [2..100], b <- [2..100]]
