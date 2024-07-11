module Problem5 (fun5) where

import Data.Map (Map, adjust, adjustWithKey, elems, empty, findWithDefault, foldrWithKey, fromList, insert, keys, member)
import SieveErastotenes (sieveErastotenes)

-- to solve puzzle, use fun5 20
fun5 = lowestCommonMultiple

-- you provide a number it will lcm for a list of numbers up to this number
lowestCommonMultiple :: Int -> Int
lowestCommonMultiple limit = lowestCommonMultiple' (findPrimeFactorsOfList [2 .. limit])

lowestCommonMultiple' :: Map Int Int -> Int
lowestCommonMultiple' map = let f k a result = result * k ^ a in foldrWithKey f 1 map

-- outputs a map of list of prime factors of list of numbers
-- ex. [2,3,4] will give fromList[(2,2),(3,1)] because 3 = 3^1, 4 = 2^2 and 2 = 2^1, see lower for explanation
findPrimeFactorsOfList :: [Int] -> Map Int Int
findPrimeFactorsOfList list = findPrimeFactorsOfList' list empty

findPrimeFactorsOfList' :: [Int] -> Map Int Int -> Map Int Int
findPrimeFactorsOfList' list map
  | list == [] = map
  | otherwise =
      let listOfTuples = findPrimeFactorsOfNumber (head list)
       in findPrimeFactorsOfList' (tail list) (addTuples listOfTuples map)

-- adds a list of tuples to map
addTuples :: [(Int, Int)] -> Map Int Int -> Map Int Int
addTuples listOfTuples map
  | listOfTuples == [] = map
  | otherwise = addTuples (tail listOfTuples) (addTupleToMap (head listOfTuples) map)

-- adds single tuple to map
addTupleToMap :: (Int, Int) -> Map Int Int -> Map Int Int
addTupleToMap tuple map =
  if member (fst tuple) map
    then adjust (max (snd tuple)) (fst tuple) map
    else insert (fst tuple) (snd tuple) map

-- function to find all prime factors of a number, returns a list of tuples
-- tuples are (number, power), ex. 12 -> [(2,2),(3,1)], because 12 = 2^2 * 3^1
findPrimeFactorsOfNumber number = findPrimeFactorsOfNumber' number (sieveErastotenes number) []

-- listOfNumbers is list of primes to check, listOfFactors is list of tuples, which are already checked
findPrimeFactorsOfNumber' :: Int -> [Int] -> [Int] -> [(Int, Int)]
findPrimeFactorsOfNumber' number listOfNumbers listOfFactors
  | listOfNumbers == [] = []
  | otherwise =
      if number `mod` head listOfNumbers == 0
        then findPrimeFactorsOfNumber' number (tail listOfNumbers) listOfFactors ++ [(head listOfNumbers, findPrimeFactorsOfNumber'' number (head listOfNumbers) 0)]
        else findPrimeFactorsOfNumber' number (tail listOfNumbers) listOfFactors

-- returns number of times 'number' can be divided by divisor
findPrimeFactorsOfNumber'' number divisor counter
  | number < 1 = error "what are you doing?"
  | number == 1 = 0
  | otherwise =
      if number `mod` divisor == 0
        then findPrimeFactorsOfNumber'' (number `div` divisor) divisor counter + 1
        else counter

-- this problem is lowest common multiple of [20,19..1]
-- for 2 numbers lcm(a,b) = abs(a*b)/gcd(a,b)
-- gcd is greatest common multiplicator
-- for more, there is prime factorization
-- for [10..1]
-- 1 = 1^1 (skip)
-- 2 = 2^1
-- 3 = 3^1
-- 4 = 2^2
-- 5 = 2^1 * 5^1
-- 6 = 2^1 * 3^1
-- 7 = 7^1
-- 8 = 2^3
-- 9 = 3^2
-- 10 = 2^1 * 5^1
-- now, we have to find factors, which occur the most
-- we find that
-- 2 is found 3 times (in 8)
-- 3 is found 2 times (in 9)
-- 5 is found 1 time (in 10 or 5)
-- 7 is found 1 time (in 7)
-- we get 2^3 * 3^2 * 5 * 7 = 2520
