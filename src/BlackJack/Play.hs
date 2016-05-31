module BlackJack.Play (
    playRound,
    endlessDeck,
) where

import System.Random

import BlackJack.Types


type Chips = Int
data Outcome = Tie | Win | Loss


endlessDeck :: Int -> [Card]
endlessDeck seed = randoms $ mkStdGen seed


playRound :: Chips -> PlayingStrategy -> [Card] -> (Chips, [Card])
playRound wager strategy deck =
    case playRoundOutcome strategy deck of
        (Win, remainingDeck) -> (2*wager, remainingDeck)
        (Tie, remainingDeck) -> (wager, remainingDeck)
        (Loss, remainingDeck) -> (0, remainingDeck)


playRoundOutcome :: PlayingStrategy -> [Card] -> (Outcome, [Card])
playRoundOutcome = undefined


dealerStrategy :: PlayingStrategy
dealerStrategy context =
    if bestScoreInHand (dealersHand context) < 17
    then Hit
    else Stand
