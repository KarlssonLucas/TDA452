module EX03 where

import Data.List
import Test.QuickCheck

--lookup :: Eq key => key -> [(key, value)] -> Maybe value
--Q1
--a
lookupAll :: Eq key => key -> [(key, value)] -> [value]
lookupAll k list = [v | (k1,v) <- list, k1 == k]

--b
lookup' :: Eq key => key -> [(key, value)] -> Maybe value
lookup' k list = undefined 

--c
update :: Eq key => key -> value -> [(key, value)] -> [(key, value)]
update k v [] = [(k,v)]
update k v ((key, value):xs) | k == key  = (k,v):xs
                             | otherwise = (key, value):(update k v xs)

--Q2
--a
data Expr  = X | Num Int | Op BinOp Expr Expr deriving (Eq, Show)
data BinOp = Add | Mul | Substract deriving (Eq, Show)

ex1 = Op Substract (Num 100) X -- 100 - x 
ex2 = Op Add (Num 100) (Op Mul (Num (-1)) X) -- 100 + (-1)*x

eval :: Expr -> Int -> Int
eval X n = n
eval (Num x) n    = x
eval (Op op e1 e2) n = evalOp op (eval e1 n) (eval e2 n)

evalOp :: BinOp -> Int -> Int -> Int
evalOp Add = (+)
evalOp Substract = (-)
evalOp Mul = (*)

--Q2
removeSub :: Expr -> Expr
removeSub (Op op e1 e2) | op == Substract = (Op Add e1 (Op Mul (Num (-1)) e2)) 
                        | otherwise = (Op op (removeSub e1) (removeSub (e2)))  

--Q3
fa :: Bool -> Bool -> Bool
fa x y = not (x && y)

fb :: (a->b->Bool) -> a -> b -> Bool
fb (&) x y = not (x & y)

fc :: Fractional a => a -> a -> a
fc x y = (x+y)/2

fd :: [[a]] -> [a]
fd x = [z | y <- x, z <-y]

--Q4
data Grid a = Grid [[a]] deriving (Eq, Show)

g1,g2 :: Grid Int 
g1 = Grid [[1,2],
           [3,4],
           [5,6]]

g2 = Grid [[5,3,1], 
           [6,4,2]]

mapGrid :: (a -> b) -> Grid a -> Grid b 
mapGrid f (Grid a) = Grid [map f x | x <- a]

