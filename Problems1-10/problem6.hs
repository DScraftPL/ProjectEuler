module Problem6 (fun6) where

-- to solve puzzle, use fun6 10
fun6 limit = sumFromTo 0 limit ^ 2 - sumOfSquares limit

-- sum of numbers from first to last
-- for sum of 1 to 10 put sumFromTo 0 10 (== 55)
sumFromTo :: (Integral a) => a -> a -> a
sumFromTo first last = (first + last) `div` 2 * (last - first + 1)

-- sum of squares to given limit number
sumOfSquares :: Int -> Int
sumOfSquares limit = sum [x * x | x <- [1 .. limit]]

-- maybe this can be faster with adding 2n+1 ?
-- (n+1)^2 - (n)^2 = (2n+1) so to get:
-- 2^2 = 1 + 3
-- 3^2 = 1 + 3 + 5
-- 4^2 = 1 + 3 + 5 + 7
-- 5^2 = 1 + 3 + 5 + 7 + 9
