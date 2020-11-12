module TB01 where

import Data.List
import Test.QuickCheck

-- maxi x y returns the maximum of x and y
maxi :: Ord a => a -> a -> a
maxi x y | x < y     = y
         | otherwise = x

-- tests could be check if it returns either x or y
-- and maybe if they return the correct

-- sumsq n returns 1*1 + 2*2 + ... + n*n
sumsq :: Integer -> Integer
sumsq n | n == 0    = 0
        | otherwise = n * n + sumsq (n-1)

-- Move n rings from leftmost to rightmost, without bigger being on smaller
hanoi :: Integer -> Integer
hanoi n | n == 0    = 0
        | otherwise = 2 * hanoi(n-1) + 1

-- Fibonacci numbers
-- F 0   = 1
-- F 1   = 1
-- F n+2 = F n+1 + F n
fibo :: Integer -> Integer
fibo n | n == 0    = 0
       | n == 1    = 1
       | otherwise = fibo (n-2) + fibo (n-1)

-- Smallestfactor n
-- smallestfactor 14 = 2
-- smalletsfactor 15 = 3
allFactors n = [x | x<-[1..n], n `mod` x == 0]
smallestfactor :: Integer -> Integer
smallestfactor n | n>1 = allFactors n !! 1

multiply :: Num a => [a] -> a
multiply []     = 0
multiply (x:xs) = x * multiply (xs)

-- duplicates :: Eq a => [a] -> Bool
-- duplicates list = nub list /= list

boolDup :: Eq a => [a] -> Bool
boolDup []   = False
boolDup (x:xs) | x `elem` xs = duplicates xs
               | otherwise = x:duplicates xs

duplicates :: Eq a => [a] -> [a]
duplicates []     = []
duplicates (x:xs) | x `elem` xs = duplicates xs
                  | otherwise   = (x:(duplicates xs))




