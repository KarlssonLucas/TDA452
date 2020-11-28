module EX04 where

import Test.QuickCheck
import System.IO

prompt p = do putStr p
              hFlush stdout    -- imported from System.IO

sumOfNumbers =
  do putStrLn "Compute the sum fo some numbers."
     prompt "How many numbers? "
     n <- readLn
     let ask n = do prompt ("Enter a number: ")
                    readLn
     ns <- mapM ask [1..n]
     putStr "The sum of the numbers is "
     print (sum ns)

sortNumber = do
        ns <- readNumbers
        print ns

readNumbers = do
    prompt "asd"
    n <- readLn 
    if n == 0 then return [] else do ns <- readNumbers 
                                     return (n:ns)

-- Number guessing, game :: IO ()


game l h = 
 do putStrLn (show ((l+h)/2))
    n <- getLine
    if n == "higher" then do n <- game (floor ((l+h)/2)) h 
                             return n
                     else do ns <- game l ((l+h)/2)
                             return ns 


