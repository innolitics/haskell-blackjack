type Hand = [Card]

type Points = Int

data Card = Card Rank Suite

data Suite = Spades | Hearts | Diamonds | Clubs

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA deriving Show


main = print $ handPointValues sampleHand
    where
        sampleHand = [Card R2 Spades, Card RA Clubs]


handPointValues :: Hand -> [Points]
handPointValues hand = 
        map sum cardPointPermutations
    where
        cardPointPermutations = sequence $ map cardPointValues hand

cardPointValues :: Card -> [Points]
cardPointValues (Card rank _) = rankPointValues rank

rankPointValues :: Rank -> [Points]
rankPointValues rank = 
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
