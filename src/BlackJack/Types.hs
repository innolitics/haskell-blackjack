module BlackJack.Types (
    PlayingStrategy(..),
    RoundState(..),
    Action(..),
    Hand(..),
    Card(..),
    Suite(..),
    Rank(..),
) where

import System.Random


type PlayingStrategy = (RoundState -> Action)

data RoundState = RoundState { playersHand, dealersHand :: Hand }

data Action = Stand | Double | Hit

type Hand = [Card]

data Card = Card { rank :: Rank, suite :: Suite }

data Suite = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Enum, Bounded)

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
    deriving (Show, Eq, Bounded, Enum)


instance Show Card where
    show (Card rank suite) = tail $ show rank ++ show suite


stride = fromEnum (maxBound :: Rank) + 1

instance Enum Card where
    fromEnum (Card rank suite) = fromEnum rank + stride*(fromEnum suite)
    toEnum n = Card (toEnum rankPosition) (toEnum suitePosition)
        where
            suitePosition = n `div` stride
            rankPosition = n `mod` stride


instance Bounded Card where
    minBound = Card minBound minBound
    maxBound = Card maxBound maxBound

instance Random Card where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                        (x, g') -> (toEnum x, g')

    random g = randomR (minBound, maxBound) g


instance Show Suite where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"
