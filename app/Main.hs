module Main where

import BlackJack.Play
import BlackJack.Types
import System.Random

main :: IO ()
main = do
    let g = mkStdGen 0
        deck = endlessDeck g
    print . take 5000 $ deck
