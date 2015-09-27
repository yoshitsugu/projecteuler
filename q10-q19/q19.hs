
data Date = Date { year :: Int, month :: Int, day :: Int, dayOfWeek :: Int} deriving (Show)

thirtyDays = [4,6,9,11]

countSundays :: Int
countSundays = count (Date 1900 1 1 1) 0
  where
    count :: Date -> Int -> Int
    count (Date 2000 12 31 _) i = i
    count x@(Date 1900 _ _ _) _ = count (next x) 0
    count x@(Date _ _ 1 7) i = count (next x) (i+1)
    count x i = count (next x) i

    isLastDayOfMonth :: Date -> Bool
    isLastDayOfMonth d =
      if (month d) == 2
      then
        if isLeapYear $ year d
        then
           (day d) == 29
        else
           (day d) == 28
      else
        if elem (month d) thirtyDays
        then
           (day d) == 30
        else
           (day d) == 31

    next :: Date -> Date
    next d@(Date x y z w) =
      if isLastDayOfMonth d
      then
        if (month d) == 12
        then
           Date (x+1) 1 1 (nextDoW w)
        else
          Date x (y+1) 1 (nextDoW w)
      else
        Date x y (z+1) (nextDoW w)

    nextDoW :: Int -> Int
    nextDoW 7 = 1
    nextDoW w = w+1


    isLeapYear :: Int -> Bool
    isLeapYear y = ((y `mod` 4) == 0) && (((y `mod` 100) /= 0) ||  ((y `mod` 400) == 0))
  
main = print countSundays

-- https://wiki.haskell.org/Euler_problems/11_to_20#Problem_19
--  problem_19 =  length . filter (== sunday) . drop 12 . take 1212 $ since1900
--  since1900 = scanl nextMonth monday . concat $
--                replicate 4 nonLeap ++ cycle (leap : replicate 3 nonLeap)
--   
--  nonLeap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
--   
--  leap = 31 : 29 : drop 2 nonLeap
--   
--  nextMonth x y = (x + y) `mod` 7
--   
--  sunday = 0
--  monday = 1
