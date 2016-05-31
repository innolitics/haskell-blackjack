module BlackJack.Play (
    endlessDeck,
) where

import System.Random

import BlackJack.Types


endlessDeck :: Int -> [Card]
endlessDeck = randoms . mkStdGen
