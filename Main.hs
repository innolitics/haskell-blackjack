type Hand = [Card]

data Card = Card Rank Suite

data Suite = Spades | Hearts | Diamonds | Clubs

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA deriving Show


main = putStrLn $ show $ handPointValues [
    Card R2 Spades,
    Card RA Clubs,
    Card R3 Hearts,
    Card RA Spades]


handPointValues :: Hand -> [Int]
handPointValues h = map sum $ sequence $ map cardPointValues h

cardPointValues :: Card -> [Int]
cardPointValues (Card d s) = rankPointValues d

rankPointValues :: Rank -> [Int]
rankPointValues r = 
    case r of
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
    show (Card d s) = tail $ show d ++ show s

instance Show Suite where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"
