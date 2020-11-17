module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

-- A0 Start -----------------------------------------------
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)
            
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
display Empty = "\n" -- Base case for recursion
display (Add card hand) = "\n" ++ displayCard card ++ display hand

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
value :: Hand -> Integer   
value h | initialValue h > 21 = initialValue h - (numberOfAces h) * 10
             | otherwise           = initialValue h

-----------------------------------------------------------

-- A3 Start -----------------------------------------------

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-----------------------------------------------------------

-- A4 Start -----------------------------------------------

winner :: Hand -> Hand -> Player
winner gh bh | gameOver gh                   = Bank -- Guest or both bust
             | gameOver bh                   = Guest -- Bank bust but not guest
             | value gh > value bh = Guest -- Player has better hand
             | otherwise                     = Bank  -- player cant win

-----------------------------------------------------------

-- B1 Start -----------------------------------------------
(<+) :: Hand -> Hand -> Hand
Empty         <+ h  = h
(Add card h1) <+ h2 = (Add card (h1 <+ h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
        p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == (size h1) + (size h2)
-----------------------------------------------------------

-- B2 Start -----------------------------------------------
ranks :: [Rank]
ranks = [Numeric n | n <- [2..10]] ++  [Jack, Queen, King, Ace]

suits :: [Suit]
suits = [Hearts, Diamonds, Spades, Clubs]

cards :: [Card]
cards = [Card rank suit | rank <- ranks, suit <- suits]

cardHelp ::  Hand -> [Card] -> Hand
cardHelp  hand [] = hand
cardHelp hand (c:cs) = (Add c (cardHelp hand cs))

fullDeck :: Hand
fullDeck = cardHelp Empty cards
-----------------------------------------------------------

-- B3 Start -----------------------------------------------
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand                        = error "draw: The deck is empty"
draw (Add card  deck) Empty            = ((deck), (Add card Empty))
draw (Add card1 deck) (Add card2 hand) = ((deck), (Add card1 (Add card2 hand)))
-----------------------------------------------------------

-- B4 Start -----------------------------------------------
playBankHelper :: Hand -> Hand -> Hand
playBankHelper Empty h = h
playBankHelper d h | value h < 16 = playBankHelper sd bh
                   | otherwise = h
                   where (sd, bh) = draw d h

playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty
-----------------------------------------------------------

-- B5 Start -----------------------------------------------

-- Note: does not preserve order of the hand
drawIndex :: Int -> (Hand, Hand) -> (Hand, Hand)
drawIndex _ (_, Empty) = error "Ran out of cards"
drawIndex 0 (h1, (Add c2 h2)) = ((Add c2 h1), h2)
drawIndex n (h1, (Add c h2)) = drawIndex (n - 1) (h1, (h2 <+ (Add c Empty)))

-- Helper for recursion
shuffleHelper :: StdGen -> (Hand, Hand) -> (Hand, Hand)
shuffleHelper g (hand, Empty) = (hand, Empty)
shuffleHelper g (hand, deck) = shuffleHelper g1 (drawIndex (n1) (hand, deck))
    where (n1, g1) = randomR (0, size deck) g

shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty = Empty
shuffleDeck g hand = fst( shuffleHelper g (Empty, hand) )

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty                   = False
c `belongsTo` (Add c1 h2) | c1 == c   = True
                          | otherwise = belongsTo c h2

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
        c `belongsTo` h == c `belongsTo` shuffleDeck g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)
-----------------------------------------------------------

-- B6 Start -----------------------------------------------
implementation = Interface
  { iFullDeck   = fullDeck
    , iValue    = value
    , iDisplay  = display
    , iGameOver = gameOver
    , iWinner   = winner 
    , iDraw     = draw
    , iPlayBank = playBank
    , iShuffle  = shuffleDeck
    }

main :: IO ()
main = runGame implementation
-----------------------------------------------------------












