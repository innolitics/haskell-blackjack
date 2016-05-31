module BlackJack.Scoring (
    bestScoreInHand,
    isHandBusted,
    handHasHighAce,
) where

import Data.List (nub, null)

import BlackJack.Types


type Score = Int


handHasHighAce :: Hand -> Bool
handHasHighAce hand = null $ filter (< bestScore') scores
    where
        bestScore' = bestScore scores
        scores = handScores hand

bestScoreInHand :: Hand -> Score
bestScoreInHand = bestScore . handScores

bestScore :: [Score] -> Score
bestScore [] = 0
bestScore scores =
    if isBusted scores then
        minimum scores
    else
        maximum $ filter validScore scores

isHandBusted :: Hand -> Bool
isHandBusted = isBusted . handScores

isBusted :: [Score] -> Bool
isBusted = null . filter validScore

validScore :: Score -> Bool
validScore = (<= 21)

handScores :: Hand -> [Score]
handScores hand = nub handScorePermutations
    where
        handScorePermutations = map sum cardScorePermutations
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
