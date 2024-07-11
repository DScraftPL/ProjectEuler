module Problem1 (fun1) where

import Data.Set (fromList, toList)

-- to solve puzzle, use fun1 1000
fun1 :: (Ord a, Enum a, Num a) => a -> a
fun1 limit = sum (joinlists listof3 listof5)
  where
    listof3 = multiplesof 3 limit
    listof5 = multiplesof 5 limit

-- create lists of multiples of multiplicator to given limit
multiplesof :: (Ord a, Enum a, Num a) => a -> a -> [a]
multiplesof multiplicator limit = takeWhile (< limit) [n * multiplicator | n <- [1 .. limit]]

-- join 2 lists w/o duplicates
joinlists :: (Ord a) => [a] -> [a] -> [a]
joinlists list1 list2 = toList (fromList (list1 ++ list2))
