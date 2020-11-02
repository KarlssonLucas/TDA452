{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k+1 


-- B -------------------------
-- power1

power2 :: Integer -> Integer -> Integer
power2 n k = product [n | i<-[1..k]]  

-- C -------------------------
-- power2

power3 :: Integer -> Integer -> Integer
power3 n k | k == 0    = 1
           | odd k     = n * power3 n (k-1)
           | even k    = power3 n (k `div` 2) * power3 n (k `div` 2)
           | otherwise = error "exception"
         

-- D -------------------------
{- 

<Describe your test cases here>

 -}

-- 
prop_powers = undefined

--
powerTest :: Bool
powerTest = undefined

--
prop_powers' = undefined


