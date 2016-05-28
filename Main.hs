data Suite = Spades | Hearts | Diamonds | Clubs

instance Show Suite where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"

data Denomination =
      Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving Show

data Card = Card {
    denomination :: Denomination,
    suite :: Suite
}

instance Show Card where
    show (Card d s) = show d ++ (show s)

type Hand = [Card]

handScore :: Hand -> [Int]
handScore h = map sum $ sequence $ map cardScore h

cardScore :: Card -> [Int]
cardScore (Card d s) = denominationScore d

denominationScore :: Denomination -> [Int]
denominationScore Two = [2]
denominationScore Three = [3]
denominationScore Four = [4]
denominationScore Five = [5]
denominationScore Six = [6]
denominationScore Seven = [7]
denominationScore Eight = [8]
denominationScore Nine = [9]
denominationScore Ten = [10]
denominationScore Jack = [10]
denominationScore Queen = [10]
denominationScore King = [10]
denominationScore Ace = [1, 11]

main = putStrLn $ show $ handScore [Card Three Spades, Card Ace Spades]
