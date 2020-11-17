module TB01 where

import Test.QuickCheck
-- 0 -- 
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

-- END --
-- 1 --
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []        = True
isPermutation (x:xs) list = x `elem` list && isPermutation xs (removecommon x list)
removecommon :: Eq a => a -> [a] -> [a]
removecommon x (y:ys) | x == y = ys
                      | otherwise = y:removecommon x ys
                       
-- END --
-- 2 --
duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (x:xs) = x `elem` xs || duplicates xs

-- END --
-- 3 -- 
pascal :: Int -> [Int]
pascal 1  = [1]
pascal n = 1 : zipWith (+) ps (tail ps)++[1]
    where ps = pascal (n-1)

-- END --
-- 4 --
crossOut :: Int -> [Int] -> [Int]
crossOut m ns = [n | n <- ns, n `mod` m /= 0]  

sieve :: [Int] -> [Int]
sieve (n:ns) = n : sieve (crossOut n ns)  
sieve []     = [] 


