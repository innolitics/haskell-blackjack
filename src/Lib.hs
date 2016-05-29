module Lib (
    handScores,
    endlessDeck,
    Suite(..),
    Rank(..),
    Card(..),
    allCards,
    ) where

import System.Random

data Action = Stand | Double | Hit

type Hand = [Card]

type Score = Int

data Card = Card { rank :: Rank, suite :: Suite }

data Suite = Clubs | Diamonds | Hearts | Spades
    deriving (Eq, Enum, Bounded)

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
    deriving (Show, Eq, Bounded, Enum)


stride = fromEnum (maxBound :: Rank) + 1

instance Enum Card where
    toEnum n = Card (toEnum rankPosition) (toEnum suitePosition)
        where
            suitePosition = n `div` stride
            rankPosition = n `mod` stride

    fromEnum (Card rank suite) =
            fromEnum rank + stride*(fromEnum suite)

instance Bounded Card where
    minBound = Card minBound minBound
    maxBound = Card maxBound maxBound

instance Random Rank where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                        (x, g') -> (toEnum x, g')

    random g = randomR (minBound, maxBound) g

instance Random Suite where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                        (x, g') -> (toEnum x, g')

    random g = randomR (minBound, maxBound) g

instance Random Card where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                        (x, g') -> (toEnum x, g')

    random g = randomR (minBound, maxBound) g

--

-- start with score 0
-- start round
    -- 1. deal two cards to player
    -- 2. deal two cards to dealer
    -- 3. if someone wins handle scoring and end
    --    else let each player decide their next action
    -- perform each action
    --

allCards :: [Card]
allCards = [minBound..maxBound]

endlessDeck :: StdGen -> [Card]
endlessDeck g = let (card, g') = random g in card : endlessDeck g'

dealerDecide :: Hand -> Action
dealerDecide hand = if bestScore hand < 17 then Hit else Stand

containsAce :: Hand -> Bool
containsAce = any (RA ==) . map rank

bestScore :: Hand -> Int
bestScore = maximum . filter validScore . handScores

validScore :: Int -> Bool
validScore = (<= 21)

handScores :: Hand -> [Score]
handScores hand =
        map sum cardScorePermutations
    where
        cardScorePermutations = sequence $ map cardScores hand

cardScores :: Card -> [Score]
cardScores (Card rank _) =
    case rank of
        R2 -> [2]
        R3 -> [3]
        R4 -> [4]
        R5 -> [5]
        R6 -> [6]
        R7 -> [7]
        R8 -> [8]
        R9 -> [9]
        RA -> [1, 11]
        _  -> [10]


instance Show Card where
    show (Card rank suite) = tail $ show rank ++ show suite

instance Show Suite where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"
