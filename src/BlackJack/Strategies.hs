module BlackJack.Strategies (
    dumbStrategy,
) where

import BlackJack.Types
import BlackJack.Scoring


dumbStrategy :: PlayingStrategy
dumbStrategy _ = Stand
