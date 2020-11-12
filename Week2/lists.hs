module TB01 where
import Test.QuickCheck
import Prelude hiding ((++), reverse, drop, take)

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]


