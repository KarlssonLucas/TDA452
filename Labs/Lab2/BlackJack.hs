module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- A0 Start -----------------------------------------------
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

hand1 :: Hand
hand1 = Add (Card (Numeric 5) Spades) (Add (Card (Numeric 6) Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                   (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty) 
            , 1 + 1 + size Empty
            , 2]

-----------------------------------------------------------

-- A1 Start -----------------------------------------------

displayCard :: Card -> String
displayCard (Card (Numeric n) suit) = show n ++  " of " ++ show suit
displayCard (Card rank suit) = show rank ++ " of " ++ show suit

display :: Hand -> String
display Empty = "" -- Base case for recursion
display (Add card hand) = displayCard card ++ "\n" ++ display hand

-----------------------------------------------------------

-- A2 Start -----------------------------------------------

-- Value of hand if ace was worth 11
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add (Card (Numeric n) _) h)   = n  + initialValue h 
initialValue (Add (Card r _) h) | r == Ace  = 11 + initialValue h 
                                | otherwise = 10 + initialValue h -- suited cards

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r _) h) | r == Ace = 1 + numberOfAces h
                                | otherwise = numberOfAces h
-- Calculates if aces should be worth 11 or 1
-- 11 - 10 = 1 therefor -10 for every ace if bust
finalValue :: Hand -> Integer   
finalValue h | initialValue h > 21 = initialValue h - (numberOfAces h) * 10
             | otherwise           = initialValue h

-----------------------------------------------------------

-- A3 Start -----------------------------------------------

gameOver :: Hand -> Bool
gameOver hand = finalValue hand > 21

-----------------------------------------------------------

-- A4 Start -----------------------------------------------

winner :: Hand -> Hand -> Player
winner gh bh | gameOver gh                   = Bank -- Guest or both bust
             | gameOver bh                   = Guest -- Bank bust but not guest
             | finalValue gh > finalValue bh = Guest -- Player has better hand
             | otherwise                     = Bank  -- player cant win

-----------------------------------------------------------

-- B1 Start -----------------------------------------------

(<+) :: Hand -> Hand -> Hand
(<+) (Add c h) Empty       = (Add c h)
(<+) (Add c1 h1) (Add c h) = (Add c (Add c1 h1)) <+ h

-----------------------------------------------------------

-- B2 Start -----------------------------------------------

suits = [Hearts, Diamonds, Clubs, Spades]
ranks = [Jack, Queen, King, Ace]

fullDeck :: Hand 
fullDeck = hand1

fdh :: [Suit] -> [Rank] -> Hand
fdh suits ranks = [ (s,r) | s <- suits, r <- ranks]


-----------------------------------------------------------
    
-- A4 Start -----------------------------------------------
-----------------------------------------------------------

-- A4 Start -----------------------------------------------
-----------------------------------------------------------

-- A4 Start -----------------------------------------------
-----------------------------------------------------------












