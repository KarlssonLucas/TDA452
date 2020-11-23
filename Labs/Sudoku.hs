module Sudoku where

import Test.QuickCheck
import Data.Char

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
rowToString (Nothing : xs) str = rowToString xs (". " ++ str)
rowToString ((Just n) : xs) str = rowToString xs ((show n) ++ " " ++ str)

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
cell = undefined


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = undefined

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = undefined
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock = undefined


-- * D2

blocks :: Sudoku -> [Block]
blocks = undefined

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
