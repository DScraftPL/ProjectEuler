module Problem4 (fun4) where

-- to solve puzzle, use fun4 3
fun4 = lookForMax

lookForMax :: Int -> (Int, Int, Int)
lookForMax digits = lookForMax' startnumber startnumber (0, 0, 0) 0 where startnumber = createMaxNumber digits

lookForMax' :: Int -> Int -> (Int, Int, Int) -> Int -> (Int, Int, Int)
lookForMax' number1 number2 foundMaxTuple counter
  | counter == 100 = foundMaxTuple
  | number2 == 0 = lookForMax' (number1 - 1) (number1 - 1) foundMaxTuple counter
  | otherwise =
      let debatedMax = number1 * number2
       in if checkPalindrom debatedMax
            then
              if trd foundMaxTuple < debatedMax
                then lookForMax' number1 (number2 - 1) (number1, number2, debatedMax) (counter + 1)
                else lookForMax' number1 (number2 - 1) foundMaxTuple (counter + 1)
            else lookForMax' number1 (number2 - 1) foundMaxTuple counter

createMaxNumber :: Int -> Int
createMaxNumber digits = read (replicate digits '9') :: Int

checkPalindrom :: Int -> Bool
checkPalindrom = checkPalindrom' . convertToList

-- ik, there is redundancy, dont care,
-- you provide list, it returns if its a palindrom or not lol
checkPalindrom' :: (Eq a) => [a] -> Bool
checkPalindrom' list
  | length list == 0 = True
  | length list == 1 = True
  | length list == 2 = if head list == last list then True else False
  | otherwise = if head list == last list then checkPalindrom' (tail (init list)) else False

-- simple conversion of number to list
convertToList :: (Integral a) => a -> [a]
convertToList number
  | number == 0 = []
  | otherwise = convertToList (number `div` 10) ++ [number `mod` 10]

-- fst, snd, had to create with pattern matching voodoo for this module
trd :: (a, b, c) -> c
trd (_, _, x) = x
