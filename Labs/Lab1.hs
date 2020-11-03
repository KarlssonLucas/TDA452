module Power where
import Test.QuickCheck

{- Lab 1
   Date: 3rd November 2020
   Authors: Jakob Ristners and Lucas Karlsson
   Lab group: 17
 -}
--------------------------------------------

power :: Integer -> Integer -> Integer
power 0 0  = error "0 to the 0th power is undefined"
power n k  | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 0 0 = error "0 to the 0th power is undefined"
power1 n k | k < 0 = error "power: negative argument"
           | otherwise = product [n | x <- [1..k]]


-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 0 0 = error "0 to the 0th power is undefined"
power2 n k | k == 0 = 1
           | k < 0 = error "power: negative argument"
           | even k = power2 (n * n) (k `div` 2)
           | odd k = n * (power n (k - 1))

-- D -------------------------
{- 
    Tests:
    1. Test 0 to the power of 0 should return an error
    2. Test 0 to the power of any positive integer is 0
    3. Test any positive Integer to the power 0 returns 1
    4. Test input of negative integer and floating point numbers
          should return an error
    5. Test that different types than an Integer return errors
 -}

-- Does not work since 0 to the 0th power and negative integers
-- return an error
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k )
              && (power1 n k == power2 n k)


-- tests the well behaved test cases are 2 and 3, they dont return errors
powerTest :: Integer -> Bool
powerTest n = (prop_powers' n 0) && (prop_powers' 0 n)

-- tests if all the power funcitons return the same value
-- for all positive integers
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = ((power absn absk == power1 absn absk) 
                && (power1 absn absk == power2 absn absk))
                where 
                    absn = abs n + 1
                    absk = abs k + 1

-- Tests everything
testAll :: Integer -> Integer -> Bool
testAll n k = prop_powers' n k && powerTest n

