module BlackJack.Strategies (
    dealerStrategy,
) where

import BlackJack.Types
import BlackJack.Scoring


dealerStrategy :: PlayingStrategy
dealerStrategy context =
    if bestScoreInHand (dealersHand context) < 17
    then Hit
    else Stand
