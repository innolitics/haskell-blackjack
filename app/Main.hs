module Main where

import Lib
import System.Random

main :: IO ()
main = do
    let g = mkStdGen 0
        deck = endlessDeck g
    print . take 5000 $ deck
