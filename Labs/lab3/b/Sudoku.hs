module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | _ <- [1..9] ] | _ <- [1..9]]

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = (length rows) == 9 && allCellsValid (Sudoku rows)

allCellsValid :: Sudoku -> Bool
allCellsValid (Sudoku [])     = True 
allCellsValid (Sudoku (x:xs)) = isValidRow x && allCellsValid (Sudoku xs) && length x == 9

isValidRow :: Row -> Bool
isValidRow []     = True
isValidRow (x:xs) = (validCell x) && isValidRow xs

validCell :: Cell -> Bool
validCell Nothing = True
validCell (Just n)  = n >= 1 && n <= 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = rowIsFilled (Sudoku rows)

rowIsFilled :: Sudoku -> Bool
rowIsFilled (Sudoku [])   = True
rowIsFilled (Sudoku (x:xs)) = not (Nothing `elem` x) && rowIsFilled (Sudoku xs)


------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do
        putStr $ sudokuToString sudoku ""

sudokuToString :: Sudoku -> String -> String 
sudokuToString (Sudoku []) str = str
sudokuToString (Sudoku (x:xs)) str = rowToString x "" ++ sudokuToString (Sudoku xs) str

rowToString :: Row -> String -> String
rowToString [] str = str ++ "\n"
rowToString (Nothing : xs) str = rowToString xs (str ++ " .")
rowToString ((Just n) : xs) str = rowToString xs (str ++ " " ++ (show n))

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do 
                s <- readFile path
                return (Sudoku (stringToSudoku [[] |_ <- [1..9] ] s))

stringToSudoku :: [Row] -> String -> [Row]
stringToSudoku rows "" = rows
stringToSudoku (r:rs) (c:cs) 
        | c == '\n' = r:(stringToSudoku rs cs) 
        | c == '.'  = stringToSudoku ((r ++ [Nothing]):rs) cs
        | otherwise = stringToSudoku ((r ++ [(Just (digitToInt c))]):rs) cs

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9, genNothing), (1, rJustInt)]

rJustInt :: Gen (Maybe Int)
rJustInt = elements [Just n | n <- [1..9]]

genNothing :: Gen (Maybe Int)
genNothing = elements [Nothing]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do 
                 rows <- vectorOf 9 $ vectorOf 9 cell
                 return (Sudoku rows)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

checkOcc :: Block -> Bool
checkOcc [] = True
checkOcc (b:bs) 
    | b == Nothing = checkOcc bs 
    | otherwise = not (b `elem` bs ) && checkOcc bs

isOkayBlock :: Block -> Bool
isOkayBlock block = isValidRow block && checkOcc block


-- * D2

block :: Sudoku -> Int -> Int -> Block
block (Sudoku rows) y x = [rows !! k !! n | n <- [x..x+2], k <- [y..y+2]]

blocks :: Sudoku -> [Block]
blocks sudoku = [block sudoku (3*y) (3*x) | y <- [0..2], x <- [0..2]]

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths (Sudoku rows) = length (blocks (Sudoku rows)) == 9 && all (==True) block
    where block = [length n == 9 | n <- (blocks $ Sudoku rows)]

-- * D3

checkOccRows :: [Row] -> Bool
checkOccRows [] = True
checkOccRows (r:rs) = checkOcc r && checkOccRows rs

getCol :: [Row] -> Int -> Row
getCol rows x = [n !! x | n <- rows]

allCols :: Sudoku -> [Row]
allCols (Sudoku rows) = [getCol rows x | x <- [0..8]]

isOkay :: Sudoku -> Bool
isOkay (Sudoku rows) = (checkOccRows $ allCols (Sudoku rows) ) 
                   &&  (checkOccRows $ blocks  (Sudoku rows))
                   &&  (checkOccRows rows)



---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) = [ (n, k) | n <- [0..8], k <- [0..8], rows !! n !! k == Nothing ]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length (blanks allBlankSudoku) == 81


-- * E2
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y)    
        | length xs == 0 = [y]
        | length xs < i = error "index larger than size of list"
        | otherwise = (take (i) xs) ++ [y] ++ (drop (i+1) xs) 


prop_bangBangEquals_correct :: (Eq a) => [a] -> (Int, a) -> Bool

prop_bangBangEquals_correct list (int, a) = ((list !!= (modint, a)) !! modint) == a
    where modint = int `mod` (length list + 1)


-- * E3

extractRow :: Sudoku -> Int -> Row
extractRow (Sudoku (r:[])) _ = r -- not sure if giving last row or error is best
extractRow (Sudoku (r:rs)) 0 = r
extractRow (Sudoku (r:rs)) i = extractRow (Sudoku rs) (i-1)  

update :: Sudoku -> Pos -> Cell -> Sudoku 
update (Sudoku rows) (i,k) y = Sudoku ((take (i) rows) ++ [(extractRow (Sudoku rows) i) !!= (k,y)] ++ (drop (i+1) rows))

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated sud (y, x) cell =
        (extractRow (update sud (yabs, xabs) cell) yabs) !! xabs == cell
        where yabs = abs y `mod` 9
              xabs = abs x `mod` 9



------------------------------------------------------------------------------

-- * F1

getBlock :: Pos -> Sudoku -> Block
getBlock (x, y) sud = undefined

possibleCells :: Pos -> Sudoku -> [Int]
possibleCells pos sud = [n | n <- [1..9], 
    isSudoku (update sud pos (Just n)),
    isOkay (update sud pos (Just n))]

selectCell :: [Int] -> (Maybe Int, [Int])
selectCell [] = (Nothing, [])
selectCell (x:xs) = ((Just x), xs)

snd' (a, b, c) = b
lst' (a, b, c) = c

solve :: Sudoku -> Maybe Sudoku
solve sud = Just $ snd' $ solveHelp ((0, 0), sud, (possibleCells (0, 0) sud))

nextPos :: Pos -> Maybe Pos
nextPos (8, 8) = Nothing
nextPos (8, x) = Just (0, x+1)
nextPos (y, x) = Just (y+1, x)

prevPos :: Pos -> Maybe Pos 
prevPos (0, 0) = Nothing
prevPos (0, x) = Just (8, x-1)
prevPos (y, x) = Just (y-1, x)

checkNext :: Pos -> Sudoku -> Bool
checkNext (8, 8) _ = True
checkNext pos sud = checkPossible (nextPos pos) sud 

checkPossible :: Maybe Pos -> Sudoku-> Bool
checkPossible Nothing _ = True
checkPossible pos sud = (length (possibleCells (fromJust pos) sud)) > 0

solveHelp :: (Pos, Sudoku, [Int]) -> (Pos, Sudoku, [Int])
solveHelp (pos, sud, []) = (pos, sud, [])
solveHelp ((8, 8), sud, cells) = ((8, 8), (update sud (8, 8) (Just (head cells))), (tail cells))
solveHelp (pos, sud, cells) | not (checkNext pos (update sud pos cell))
                            = solveHelp (pos, sud, newcells)
                            | otherwise = solveHelp ((fromJust $ nextPos pos), (update sud pos cell), (possibleCells (fromJust (nextPos pos)) (update sud pos cell))) where (cell, newcells) = selectCell cells


-- * F3


-- * F4
