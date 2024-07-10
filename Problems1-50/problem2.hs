module Project2 (fun2) where

-- to solve puzzle, use fun2 4000000
fun2 :: (Integral a) => a -> a
fun2 limit = sum (filter even (fibonacciFromScratch startlimit limit)) where startlimit = [1, 2]

fibonacciFromScratch :: (Ord a, Num a) => [a] -> a -> [a]
fibonacciFromScratch list limit
  | last list > limit = init list
  | otherwise = fibonacciFromScratch (list ++ [last list + last (init list)]) limit
