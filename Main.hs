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

main = putStrLn $ show $ Card Three Spades
