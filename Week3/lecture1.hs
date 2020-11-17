module TB3A where

import Test.QuickCheck

copyFile :: FilePath -> FilePath -> IO()
copyFile fromFile toFile =
    do c <- readFile fromFile
       writeFile toFile c


longest :: IO String 
longest = do
        wlist <- readFile "/usr/share/dict/words"
        return (long wlist) 
    
        where long :: String -> String 
              long = snd . maximum . map (\w -> (length w, w)) . words


dotwice :: IO a -> IO (a,a)
dotwice i = do
        a1 <- i
        a2 <- i
        return (a1,a2)

dont :: IO a -> IO ()
dont i = return ()

test :: IO Int
test = do
    ans <- return 42
    return 0

mySequence_ :: [IO a] -> IO ()
mySequence_ []     = return () 
mySequence_ (i:is) =
    do i
       mySequence_ is



mySequence :: [IO a] -> IO [a]
mySequence [] = return []
mySequence (i:is) =
    do a <- i
       as <- mySequence is
       return (a:as)



