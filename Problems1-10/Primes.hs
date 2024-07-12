module Primes (primeListTo) where

import Data.List

primeListTo :: Integer -> [Integer]
primeListTo limit = primeListTo' [2 .. limit]

primeListTo' :: [Integer] -> [Integer]
primeListTo' (x : xs)
  | null (x : xs) = []
  | otherwise = x : primeListTo' (xs \\ [x * x, x * (x + 1) ..])
