module EX02 where

import Data.List
import Data.Maybe
import Data.Char
import Test.QuickCheck

type Binero = [[Cell]]
data Cell = Blank | Zero | One  deriving (Eq, Show)

example :: Binero
example = [[b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b],
           [b,b,b,b,b,b,b,b,b,b]]
            where b = Blank
                  o = One
                  z = Zero

-- Q1
isBinero :: Binero -> Bool
isBinero rows = and [length x == 10 | x <- rows]

-- Q2
instance Arbitrary Cell where
    arbitrary = cell

genBiner :: Gen (Binero)
genBiner = vectorOf 10 $ vectorOf 10 cell

cell :: Gen (Cell)
cell = frequency [(6, genBlank), (4, genOneZero)]

genBlank :: Gen (Cell)
genBlank = elements [Blank]

genOneZero :: Gen (Cell)
genOneZero = elements [One, Zero]

-- Q3

checkOcc :: [Cell] -> Cell -> Int
checkOcc bin c = length [x | x <- bin, x == c]

isRuleOne :: [Cell] -> Int -> Bool
isRuleOne row n = row !! n /= Blank && row !! n == row !! (n-1) && row !! (n+1) == row !! n

isOkay :: Binero -> Bool
isOkay bin = (not $ or [isRuleOne x n | x <- bin, n <- [1..8]]) && (not $ or 
                       [isRuleOne x n | x <- (transpose bin), n <- [1..8]]) && 
                   and [checkOcc x One == checkOcc x Zero | x <- bin] && 
                   and [checkOcc x One == checkOcc x Zero | x <- (transpose bin)]

isSolved :: Binero -> Bool
isSolved bin = isOkay bin && isBinero bin 

--Q4
printBin' :: Binero -> IO ()
printBin' bin = do putStrLn $ concat $ map rowBin [(c,"") | c <- bin] 
                            where rowBin :: ([Cell],String) -> String
                                  rowBin ([],s)         = s ++ "\n"
                                  rowBin ((x:xs),s) | x == Blank = rowBin (xs, s++"-")
                                                    | x == One   = rowBin (xs, s++"1")
                                                    | x == Zero  = rowBin (xs, s++"0")

-- PART 2
-- Q1
type Doc = [Item] 
data Item = Text String | DList [(String,Doc)]  deriving (Eq,Show)

mydoc = [ Text "This is my document, designed to illustrate the following:"
         ,DList [("1. ", simple "Lists of items like this one")
                ,("2. ", [Text "As well as nested lists like this one:"
                         ,DList [
                                 (bullet, simple "This is the first bullet point")
                                ,(bullet, simple "This is the second bullet point")
                                ]
                           ]
                  )
                 ]
        ]

   where simple s = [Text s]
         bullet   = "* " -- a unicode bullet character might be nicer

mapDoc :: (String -> String) -> (String -> String) -> Doc -> Doc
mapDoc mapText mapBullets [] = []
mapDoc mT mB doc             = map (dMap mT mB) doc
    where (dMap mT mB (Text s)) = undefined


