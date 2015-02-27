module Heuristics where

import Board
import AI
import AIDB
import Data.Maybe
import Data.Functor

-- |Represents the board after a move and the column used to make it
data Move = Move{ moveColumn::Int, moveBoard::Board}

getNextMoveByHeuristic:: Board -> IO Int
getNextMoveByHeuristic curBoard = getMaxColumnByHeuristic nextBoards
	where 	nextBoards = map (\(i,board) -> Move i board) $ computePossibleBoardStatesAfterTurn Player curBoard
		getMaxColumnByHeuristic boards = do
			heuristics <- mapM getTotalHeuristic boards
			let boardsAndHeuristics = zip boards heuristics
			let maxBoardAndHeuristic = foldl1 (\bph1@(_,heuristic1) bph2@(_,heuristic2) -> if heuristic1 > heuristic2 then bph1 else bph2) boardsAndHeuristics
			return $ moveColumn $ fst $ maxBoardAndHeuristic


-- |Get how good a move is by heuristics
getTotalHeuristic:: Move -> IO Int
getTotalHeuristic move = sum <$> applyHeuristics move

applyHeuristics:: Move -> IO [Int]
applyHeuristics move = do
	let pure = map ($ move) pureHeuristicList
	ioList <- mapM ($ move) iOHeuristicList
	return (pure ++ ioList)
	
applyPureHeuristics:: Move -> [Int]
applyPureHeuristics move = map ($ move) pureHeuristicList

pureHeuristicList:: [(Move -> Int)]
pureHeuristicList = [isWin, isLoss,middleDistance,maxAmountOfRuns] 

iOHeuristicList:: [(Move -> IO Int)]
iOHeuristicList = [lookupValue]

isWin (Move _ board) 
	| (hasWon board) = 100
	| otherwise = 0

isLoss (Move _ board)
	| (hasLost board) = -100
	| otherwise = 0

maxAmountOfRuns (Move _ board) = 2 *  (sum $ getAllRuns Player board)

lookupValue:: Move -> IO Int
lookupValue (Move _ board) = getValue board >>= (\b -> if isNothing b then return 0 else return $ fromJust b)

middleDistance:: Move -> Int
middleDistance (Move i _) = minimum $ [abs (4 - i), abs (3 - i)]
