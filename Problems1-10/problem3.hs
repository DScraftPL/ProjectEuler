module Problem3 (fun3) where

import Data.List (sort)
-- a cool module for primes, used to check answers
import Data.Numbers.Primes (primeFactors)
import SieveErastotenes (sieveErastotenes)

-- to solve puzzle, use fun3 600851475143 0 or fun3 600851475143 1
fun3 :: (Eq a, Num a) => Integer -> a -> [Integer] -> [Integer]
fun3 number solution
  | solution == 0 = max (factorsOfNumber number)
  | solution == 1 = max (primeFactorsOf number)
  | otherwise = error "bad number (solution)"

-- using a list of primes up to 10000, function returns a list of primes which divide a number
factorsOfNumber :: Integer -> [Integer]
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

-- left this for archive, moved to SieveErastotenes.hs
{-
-- sieve returns a list of primes up to given limit
sieveErastotenes :: (Integral a) => a -> [a]
sieveErastotenes limit = sieveErastotenes' [2 ..] limit []

-- school-taught sieve to create a list of prime numbers
sieveErastotenes' :: (Integral a) => [a] -> a -> [a] -> [a]
sieveErastotenes' list limit primelist
  | limit < head list = sort primelist
  | otherwise = sieveErastotenes' (filter (\x -> mod x (head list) /= 0) (tail list)) limit (primelist ++ [head list])
-}
