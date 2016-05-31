module BlackJack.Strategies (
    dealerStrategy,
) where

import BlackJack.Types
import BlackJack.Scoring


dumbStrategy :: PlayingStrategy
dumbStrategy _ = Stand
