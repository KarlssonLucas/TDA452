module TB01 where

import Test.QuickCheck

take' :: Int -> [a] -> [a]
take' n _      | n <= 0 = []
take' _ []              = []
take' n (x:xs)          = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n list     | n <= 0 = list
drop' _ []             = []
drop' n (x:xs)         = drop' (n-1) xs 

splitAtt :: Int -> [a] -> ([a], [a])
splitAtt n list = (take' n list, drop' n list)

zip3 :: [a] -> [a] -> [a] -> [a]
zip3 (x:xs) (y:ys) (z:zs) = 
