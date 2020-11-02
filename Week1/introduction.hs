module Test where
import Test.QuickCheck

-- Variables --
vari :: Double
vari = 12.3

-- Functions --
square :: Double -> Double
square n = n * n


m ~== n = abs (m-n) < epsilon
    where epsilon = 10e-10

abs' :: Integer -> Integer
abs' n | n<0       = -n
       | otherwise = n

power:: Integer -> Integer -> Integer
power n k | k == 0 = 1
          | k >  0 = n * power n (k-1)
          | otherwise = error "exception"

-- Tuples --
ex1 :: Integer -> (Bool, Integer)
ex1 n = (n<0, abs n)

{- 
 - fst (x,y) = x
 - snd (x,y) = Y
 -}

t1 :: (Integer, Bool, Double)
t1 = (2, False, 2.0)
removesnd (n,o,p) = (power 2 n, p)



-- Properties for testing  --
prop_sq sq = (square sq) == (sq*sq)
prop_abs n = (abs n) == (if n >= 0 then n else -n)
prop_pow n k = n^k' == power n k' where k' = abs k

