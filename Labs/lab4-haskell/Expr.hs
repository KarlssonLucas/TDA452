module Expr where

import Test.QuickCheck
import Parsing

data Expr
    = Num Double 
    | Add Expr Expr
    | Mul Expr Expr
    | Sin Expr 
    | Cos Expr 
    | X
    deriving (Eq,Show)

x :: Expr
x = X

num :: Double -> Expr
num n = Num n 

mul :: Expr -> Expr -> Expr
mul ex1 ex2 = Mul ex1 ex2

add :: Expr -> Expr -> Expr
add ex1 ex2 = Add ex1 ex2

cos :: Expr -> Expr 
cos ex1 = Cos ex1

sin :: Expr -> Expr
sin ex1 = Sin ex1

size :: Expr -> Int
size (Num n) = 1
size X       = 1
size (Sin e) = 1 + size e
size (Cos e) = 1 + size e
size (Mul e1 e2) = 1 + size e1 + size e2
size (Add e1 e2) = 1 + size e1 + size e2

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr X       = "x" 
showExpr (Sin e) = "sin" ++ convert (Sin e) e
showExpr (Cos e) = "cos" ++ convert (Cos e) e
showExpr (Mul e1 e2) = convert (Mul e1 e2) e1 ++ "*" ++ convert (Mul e1 e2) e2
showExpr (Add e1 e2) = convert (Add e1 e2) e1 ++ "+" ++ convert (Add e1 e2) e2

higherPrec :: Expr -> Expr -> Bool
higherPrec (Mul e11 e12) (Add e21 e22) = True
higherPrec (Sin e1) (Mul e21 e22) = True
higherPrec (Sin e1) (Add e21 e22) = True
higherPrec (Cos e1) (Mul e21 e22) = True
higherPrec (Cos e1) (Add e21 e22) = True
higherPrec e1 e2 = False

convert :: Expr -> Expr -> String
convert e1 e2
  | e1 `higherPrec` e2 = "(" ++ showExpr e2 ++ ")"
  | otherwise = showExpr e2

eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval X x = x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Sin e)     x = Prelude.sin (eval e x)
eval (Cos e)     x = Prelude.cos (eval e x)

expr, term, factor :: Parser Expr

expr = do
    t <- term
    ts <- zeroOrMore (do char '+'; term)
    return $ foldl Add t ts
            
term = do
    t <- factor
    ts <- zeroOrMore (do char '*'; factor)
    return $ foldl Mul t ts


factor = Num <$> readsP
    <|> do 
        char 's'
        char 'i'
        char 'n'
        e <- factor
        return $ Sin e
    <|> do
        char 'c'
        char 'o'
        char 's'
        e <- factor
        return $ Cos e
    <|> do 
        char '('
        e <- expr
        char ')'
        return e
    <|> do
        char 'x'
        return X

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (' ':xs) = removeSpaces xs
removeSpaces (x:xs) = x : removeSpaces xs
        
readExpr :: String -> Maybe Expr
readExpr str = do
    (e1, str) <- parse expr $ removeSpaces str
    return e1

fromJust :: Maybe Expr -> Expr
fromJust (Just e) = e

assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3)  = assoc (Add e1 (Add e2 e3))
assoc (Add e1 e2) = Add (assoc e1) (assoc e2)
assoc (Mul (Mul e1 e2) e3) = assoc (Mul e1 (Mul e2 e3))
assoc (Mul e1 e2) = Mul (assoc e1) (assoc e2)
assoc (Sin e) = Sin $ assoc e
assoc (Cos e) = Cos $ assoc e
assoc X = X
assoc (Num n) = Num n

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = (showExpr $ fromJust $ readExpr $ showExpr e) == showExpr e

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1, arbNumber), (s, arbBinary s),  (s, arbFunction s)]
    where arbNumber = elements $ X : map Num [-n, n]
          arbBinary s = do
              bin <- elements [Mul, Add]
              e1  <- arbExpr $ s `div` 2
              e2  <- arbExpr $ s `div` 2
              return $ bin e1 e2
          
          arbFunction s = do
              f <- elements [Sin, Cos]
              e <- arbExpr $ s `div` 2
              return $ f e

          n = 7
  
instance Arbitrary Expr where
    arbitrary = sized arbExpr

simplify :: Expr -> Expr

simplify (Num n) = (Num n)
simplify X = X

simplify (Mul (Num 0) _) = Num 0
simplify (Mul _ (Num 0)) = Num 0

simplify (Mul e (Num 1.0)) = e
simplify (Mul (Num 1.0) e) = e


simplify (Mul e1 X) = Mul (simplify e1) X
simplify (Mul X e1) = Mul X (simplify e1)
simplify (Mul (Num n1) (Num n2)) = Num (n1 * n2)

--simplify (Mul e1 (Add e2 e3)) = simplify (Add (simplify (Mul (simplify e1) (simplify e2))) (simplify (Mul (simplify e1) (simplify e3))))
--simplify (Mul (Add e2 e3) e1) = simplify (Add (simplify (Mul (simplify e1) (simplify e2))) (simplify (Mul (simplify e1) (simplify e3))))
-- These assure removal of all parethesis but give way to some very small floating
-- point errors in our quickCheck property. If you want to use these make sure to 
-- change == to =~ in the quickCheck property


simplify (Mul e1 e2) 
  | se1 == e1 && se2 == e2 = Mul e1 e2
  | se1 == e1 = simplify $ Mul e1 se2
  | se2 == e2 = simplify $ Mul se1 e2
  | otherwise = simplify $ Mul se1 se2
    where se1 = simplify e1
          se2 = simplify e2

simplify (Add X (Num 0)) = X
simplify (Add (Num 0) X) = X
simplify (Add X X) = Mul (Num 2) X


simplify (Add e1 X) = Add (simplify e1) X
simplify (Add X e1) = Add X (simplify e1)
simplify (Add (Num n1) (Num n2)) = Num (n1 + n2)
simplify (Add e1 e2) 
  | se1 == e1 && se2 == e2 = Add e1 e2
  | se1 == e1 = simplify $ Add e1 se2
  | se2 == e2 = simplify $ Add se1 e2
  | otherwise = simplify $ Add se1 se2
    where se1 = simplify e1
          se2 = simplify e2

simplify (Sin (Num n)) = Num $ Prelude.sin n
simplify (Cos (Num n)) = Num $ Prelude.cos n
simplify (Sin e) 
  | se == e = Sin e
  | otherwise = simplify $ Sin se
    where se = simplify e

simplify (Cos e) 
  | se == e = Cos e
  | otherwise = simplify $ Cos se
    where se = simplify e


prop_Simplify :: Expr -> Bool
prop_Simplify e = eval (simplify e) 1 == eval e 1

(=~) :: Double -> Double -> Bool
d1 =~ d2 = abs (d2 - d1) < 0.0001 -- wierd floating point errors when it simplifies a lot

differentiate, differentiate' :: Expr -> Expr

differentiate e = simplify $ differentiate' $ simplify e 

differentiate' (Sin e1) = Mul (differentiate e1) (Cos e1)
differentiate' (Cos e1) = Mul (differentiate e1) (Mul (Num (-1)) (Cos e1))

differentiate' (Add e1 e2) = Add (differentiate e1) (differentiate e2)
differentiate' (Mul e1 e2) = Add (Mul (differentiate e1) e2) (Mul e1 (differentiate e2))

differentiate' X = Num 1
differentiate' (Num n) = Num 0

