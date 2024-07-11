module SieveErastotenes (sieveErastotenes) where

import Data.List (sort)

-- sieve returns a list of primes up to given limit
sieveErastotenes :: (Integral a) => a -> [a]
sieveErastotenes limit = sieveErastotenes' [2 ..] limit []

-- school-taught sieve to create a list of prime numbers
sieveErastotenes' list limit primelist
  | limit < head list = sort primelist
  | otherwise = sieveErastotenes' (filter (\x -> mod x (head list) /= 0) (tail list)) limit (primelist ++ [head list])
