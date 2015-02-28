module Heuristics where

import Board
import Data.Maybe
import Data.Functor

-- |Represents the board after a move and the column used to make it
data Move = Move{ moveColumn::Int, moveBoard::Board}


getHeuristicOfBoard:: Board -> Int
getHeuristicOfBoard board = sum $ map ($ Move 0 board) pureHeuristicList
	
applyPureHeuristics:: Move -> [Int]
applyPureHeuristics move = map ($ move) pureHeuristicList

pureHeuristicList:: [(Move -> Int)]
pureHeuristicList = [maxAmountOfRuns] 

maxAmountOfRuns (Move _ board) = sum $ take 5 $  getAllRuns Player board

middleDistance:: Move -> Int
middleDistance (Move i _) = abs (4 - i)
