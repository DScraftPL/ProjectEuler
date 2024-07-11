module Problem7 (fun7) where

import SieveErastotenes (sieveErastotenes)

-- fuck this problem, primes are fuckin stupid :)
fun7 = (sieveErastotenes 200000) !! 10001
