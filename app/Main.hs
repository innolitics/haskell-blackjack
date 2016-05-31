module Main where


import BlackJack.Play
import BlackJack.Scoring


main :: IO ()
main = do
    let deck = endlessDeck 0
    print . bestScoreInHand . take 2 $ deck
    print . take 100 $ deck
