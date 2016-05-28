data Suite = Spades | Hearts | Diamonds | Clubs

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA deriving Show

data Card = Card { rank :: Rank, suite :: Suite }

type Hand = [Card]

scoreHand :: Hand -> [Int]
scoreHand h = map sum $ sequence $ map scoreCard h

scoreCard :: Card -> [Int]
scoreCard (Card d s) = scoreRank d

scoreRank :: Rank -> [Int]
scoreRank R2 = [2]
scoreRank R3 = [3]
scoreRank R4 = [4]
scoreRank R5 = [5]
scoreRank R6 = [6]
scoreRank R7 = [7]
scoreRank R8 = [8]
scoreRank R9 = [9]
scoreRank RA = [1, 11]
scoreRank _  = [10]

main = putStrLn $ show $ scoreHand [Card R3 Spades, Card RA Spades]

instance Show Suite where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"

instance Show Card where
    show (Card d s) = tail $ show d ++ show s
