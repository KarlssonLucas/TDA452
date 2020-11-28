module TB01 where

import Test.QuickCheck

nats :: Gen Integer
nats = do
    n <- arbitrary 
    return (abs n)
