module L01B where
    
data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Show,Eq)

colour :: Suit -> Colour
colour Spades = Black
colour Clubs  = Black
colour _     = Red


data Colour = Red | Black
    deriving (Show, Eq)

data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

prop_Rank (Numeric n) = n > 1 && n <= 10
prop_Rank _           = True

rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

prop_rankBeats r1 r2 = rankBeats r1 r2 || rankBeats r2 r1 || r1 == r2

data Card = Card Rank Suit
    deriving (Show, Eq)

cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1 == s2 && r1 `rankBeats` r2

suit :: Card -> Suit
suit (Card _ s) = s

rank :: Card -> Rank
rank (Card r _) = r

data Hand = Empty | Add Card Hand
    deriving Show

size :: Hand -> Int
size Empty     = 0
size (Add c h) = 1 + size h

lucas = Card King Hearts
jakob = Card King Spades

ex1 = Add lucas Empty
ex2 = Add jakob ex1

handBeats :: Hand -> Hand -> Bool
handBeats Empty   c             = False
handBeats (Add c h) (Add c1 h1) = c `handBeats`c1
