module Problem5 (fun5) where

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

fun5 = 0

findPrimeFactors = 0
