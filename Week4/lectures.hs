module TB01 where

import Test.QuickCheck

-- Recursive Types --
data Expr
    = Num Integer
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq) 

-- (1+2)
ex1 = (Add (Num 1) (Num 2)) 
ex2 = (Mul (Add (Num 1) (Num 2)) (Num 4))

eval :: Expr -> Integer
eval (Num n)     = n
eval (Mul e1 e2) = ((eval e1) * (eval e2))
eval (Add e1 e2) = (eval e1 + eval e2)

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showExpr e1 ++ " * " ++ showExpr e2

-- Overloading show
instance Show Expr where
    show = showExpr

-- Generate random expressions for recursive data
range = 4

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (s, rBin s)]
        where 
            rNum   = elements $ map Num [-range..range]
            rBin s = do
                let s' = (s `div` 2)
                op <- elements [Mul, Add]
                e1 <- rExpr s' 
                e2 <- rExpr s' 
                return $ op e1 e2

-- Make quickcheck be able to use them
instance Arbitrary Expr where
    arbitrary = sized rExpr



