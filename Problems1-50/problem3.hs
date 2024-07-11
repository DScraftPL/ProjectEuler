module Problem3 (fun3) where

import Data.List (sort)
import Data.Numbers.Primes (primeFactors)

-- to solve puzzle, use fun3 600851475143 0 or fun3 600851475143 1
fun3 number solution
  | solution == 0 = max (factorsOfNumber number)
  | solution == 1 = max (primeFactorsOf number)
  | otherwise = error "bad number (solution)"

factorsOfNumber number = factorsOfNumber' number [] (sieveErastotenes 10000)

-- brute-forcy method of finding all prime factors of a number
factorsOfNumber' :: Integer -> [Integer] -> [Integer] -> [Integer]
factorsOfNumber' workingNumber listOfFactors listOfNumbers
  | null listOfNumbers = listOfFactors
  | workingNumber == 1 = listOfFactors
  | otherwise =
      if mod workingNumber (head listOfNumbers) == 0
        then factorsOfNumber' (workingNumber `div` head listOfNumbers) (listOfFactors ++ [head listOfNumbers]) (tail listOfNumbers)
        else factorsOfNumber' workingNumber listOfFactors (tail listOfNumbers)

-- sieving is too resource heavy, swapping 'sieveErastotenes number' for 'primeFactors number' works fast (or put 10000)
primeFactorsOf number = filter (\x -> mod number x == 0) (sieveErastotenes 10000)

sieveErastotenes limit = sieveErastotenes' [2 ..] limit []

sieveErastotenes' list limit primelist
  | limit < head list = sort primelist
  | otherwise = sieveErastotenes' (filter (\x -> mod x (head list) /= 0) (tail list)) limit (primelist ++ [head list])
