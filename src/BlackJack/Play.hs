module BlackJack.Play (
    handScores,
    endlessDeck,
    ) where

import System.Random

import BlackJack.Types


endlessDeck :: StdGen -> [Card]
endlessDeck g = let (card, g') = random g in card : endlessDeck g'

dealerDecide :: Hand -> Action
dealerDecide hand = if bestScore hand < 17 then Hit else Stand

containsAce :: Hand -> Bool
containsAce = any (RA ==) . map rank

bestScore :: Hand -> Int
bestScore = maximum . filter validScore . handScores

validScore :: Int -> Bool
validScore = (<= 21)

handScores :: Hand -> [Score]
handScores hand =
        map sum cardScorePermutations
    where
        cardScorePermutations = sequence $ map cardScores hand

cardScores :: Card -> [Score]
cardScores (Card rank _) =
    case rank of
        R2 -> [2]
        R3 -> [3]
        R4 -> [4]
        R5 -> [5]
        R6 -> [6]
        R7 -> [7]
        R8 -> [8]
        R9 -> [9]
        RA -> [1, 11]
        _  -> [10]
