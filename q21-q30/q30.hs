import Control.Applicative ((<$>), (<*>))
import Data.Char (digitToInt)

sumOfFifthPowers :: Integer -> Integer
sumOfFifthPowers = sum . map ((^ 5) . fromIntegral . digitToInt) . show

limit :: Integer
limit = snd $ head $ dropWhile (\(a,b) -> a > b) $ zip (map (9^5*) [1..]) (map (10^) [1..])

main :: IO ()
main = print . sum $ filter ((==) <$> id <*> sumOfFifthPowers) [2..limit]
