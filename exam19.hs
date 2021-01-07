module EXAM01 where

import Test.QuickCheck
import Data.List

-- Q1
fa :: String -> String 
fa x = "Hello, " ++x

fb :: [a] -> (a,[a])
fb (x:y) = (x,y)

fc :: Ord a => a -> a -> a -> Bool
fc x y z = x<=y && y<=z

fd :: (a -> b) -> [[a]] -> [[b]]
fd = map . map

--Q2
--a
subseq :: [a] -> [[a]]
subseq []     = [[]]
subseq (l:ls) = [x | x' <- subseq ls, x <- [x',l:x']]
--b
isSubSeq :: Eq a => [a] -> [a] -> Bool
isSubSeq [] _ = True
isSubSeq _ [] = False
isSubSeq (l:ls) (w:ws) | l == w = isSubSeq ls ws 
                       | not $ l `elem` ws = False
                       | otherwise = isSubSeq (l:ls) (ws)

--c
prop_subSeq :: Eq a => [a] -> Property
prop_subSeq seq = length seq<15 ==> and [x `isSubSeq` seq | x <- subseq seq] 

--Q3
--a
data Equation = Eqn Expr Expr                   deriving (Eq, Show)
data Expr     = Num Int | Op Oper Expr Expr     deriving (Eq, Show)
data Oper     = Add | Sub | Mul                 deriving (Eq, Show)

--b
rExpr :: Int -> Gen Expr
rExpr 0 = Num <$> choose (1,10)
rExpr n = do op <- elements [Add,Sub,Mul]
             m  <- choose (0,n-1)
             e1 <- rExpr m
             e2 <- rExpr (n-1-m)
             return (Op op e1 e2)

--c
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [x] = [Num x]
exprs xs  = [ Op op e1 e2 | op <- [Mul, Add, Sub], (e1, e2) <- exprPair xs]

exprPair :: [Int] -> [(Expr, Expr)]
exprPair []     = []
exprPair list = [(e1,e2) | x <- [1..length list -1], e1 <- exprs (fst (splitAt x list)), e2 <- exprs (snd (splitAt x list))]

--d
equations :: [Int] -> [Equation]
equations []  = []
equations xs  = [Eqn e1 e2 | op <- [Mul, Add, Sub], (e1,e2) <- exprPair xs]

--4 
data Tree a b = Leaf a | Node b (Tree a b) (Tree a b)
--a 
mapTree :: (a1->a2) -> (b1->b2) -> Tree a1 b1 -> Tree a2 b2
mapTree l _ (Leaf a) = Leaf (l a)
mapTree l n (Node a t1 t2) = Node (n a) (mapTree l n t1) (mapTree l n t2) 

--b
foldTree :: Tree a (a->a->a) -> a
foldTree (Leaf a) = a
foldTree (Node f t1 t2) = f (foldTree t1) (foldTree t2)

--c
eval :: Expr -> Int
eval (Num x) = x
eval (Op op e1 e2) = evalOp op (eval e1) (eval e2)

evalOp :: Oper -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)

--Q5
sortFiles :: [FilePath] -> IO ()
sortFiles paths = do 
                     ls <- mapM readFile paths
                     let a = [ sort x | x <- ls] 
                     putStr $ unlines $ concatMap lines a 


