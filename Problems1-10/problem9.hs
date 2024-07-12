module Problem9 (fun9) where

-- to solve puzzle, use fun9
fun9 = product list * (sqrt (head list ^ 2 + last list ^ 2))
  where
    list = filter nonFractional (filter (> 0) [temp x | x <- [1 .. 1000]])

-- a^2 + b^2 = c^2
-- c > b > a > 0
-- a + b + c = 1000
-- a + b = 1000 - c
-- a^2 + b^2 + 2ab = 1000000 + c^2 - 2000c
-- 2ab = 1000000 - 2000c
-- ab = 500000 - 1000c
-- ab = 1000(500-c)
-- c = 500 - ab/1000
--
-- a + b + 500 - ab/1000 = 1000
-- a + b - ab/1000 = 500
--
-- a - ab/1000 = 500 - b
-- a(1-b/1000) = 500 -b
-- a = 1000*(500 - b)/(1000-b)
--
--
nonFractional d = (fromIntegral $ ceiling d) == d

temp x = 1000 * (500 - x) / (1000 - x)
