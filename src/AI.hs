module AI where



import Board
import Heuristics
import System.Random
import Data.Maybe
import Control.Parallel.Strategies

getRandom:: IO Int
getRandom = randomRIO (1,7)


maxDepth = 7

getMaxOfTree = getMaxAB

getmmOfTree = getMaxOfTreeBy miniMaxPlayer

getABOfTree = getMaxOfTreeBy miniMaxABWithSeed

getMaxOfTreeBy:: (Board -> Int -> Int) -> Board -> Int
getMaxOfTreeBy mmFunc board = indexOfMax $ parMap rdeepseq (\(i,b) -> (i, mmFunc b maxDepth)) $ computePossibleBoardStatesAfterTurn Player board
	where indexOfMax xs = fst $ foldl1 (\test1@(i1,heuristic1) test2@(i2,heuristic2) -> if heuristic1 > heuristic2 then test1 else test2) xs


getMaxAB::Board -> Int
getMaxAB board = fstThree $ foldl (\cur@(_,alpha,beta) (i,b) -> let mmAB = runEval $ rpar$ miniMaxAB b maxDepth alpha beta False in if mmAB > alpha then (i,mmAB,beta) else cur) (0,minBound,maxBound) $ computePossibleBoardStatesAfterTurn Player board
	where indexOfMax xs = fst $ foldl1 (\test1@(i1,heuristic1) test2@(i2,heuristic2) -> if heuristic1 > heuristic2 then test1 else test2) xs


--This should start on the opponents turn because you consider the best of your opponents next turns
miniMaxPlayer board depth = miniMax board depth True

miniMax board depth isMaximizer 
	| (depth == 0) = getHeuristicOfBoard board
	| (hasWon board) = maxBound
	| (hasLost board) = minBound
	| (isMaximizer) =  maximum  $ map (\b -> miniMax b (depth - 1) False) nextPlayerBoards
	| (not isMaximizer) = minimum $ map (\b -> miniMax b (depth - 1) True) nextOpponentBoards
	where 	nextPlayerBoards = map snd $computePossibleBoardStatesAfterTurn Player board
		nextOpponentBoards = map snd $computePossibleBoardStatesAfterTurn Opponent board		


miniMaxABWithSeed board depth = miniMaxAB board depth minBound maxBound False

miniMaxAB:: Board -> Int -> Int -> Int -> Bool -> Int
miniMaxAB board depth alpha beta isMaximizer 
	| (depth == 0) = sum $ getAllRuns Player  board --Time for guessing
	| (hasWon board) = maxBound --That's good
	| (hasLost board) = minBound --That's bad
	| (isMaximizer) = pruneMax alpha beta nextPlayerBoards
	| (not isMaximizer) = pruneMin alpha beta nextOpponentBoards
		where 	pruneMax:: Int -> Int -> [Board] -> Int
			pruneMax alpha beta children =fstThree $ foldl foldFunction (alpha,minBound,False) children
				where 	foldFunction (preV,prevAlpha,done) board = if done then (preV,0,True) else if beta <= newAlpha then (newV,newAlpha,True) else (newV,newAlpha,False) 
						where	newV = max preV (miniMaxAB board (depth -1) alpha beta False)
							newAlpha = max alpha newV
			pruneMin:: Int -> Int -> [Board] -> Int
			pruneMin alpha beta children  = fstThree $foldl foldFunction (beta,maxBound,False) children
				where foldFunction (preV,prevBeta,done) board =  if done then (preV,0,True) else if newBeta <= alpha then (newV,newBeta,True) else (newV,newBeta,False)
						where 	newV = min preV (miniMaxAB board (depth -1) alpha beta True)
							newBeta = min beta newV


			nextPlayerBoards = map snd $computePossibleBoardStatesAfterTurn Player board
			nextOpponentBoards = map snd $computePossibleBoardStatesAfterTurn Opponent board		


fstThree (x,_,_) = x 
