module AI where



import Board
import Heuristics
import System.Random
import Data.Maybe
import Control.Parallel.Strategies

getRandom:: IO Int
getRandom = randomRIO (1,7)


maxDepth = 5

getMaxOfTree::Board->Int
getMaxOfTree = getmmOfTree

getmmOfTree::Board -> Int
getmmOfTree = getMaxOfTreeBy miniMaxPlayer

getABOfTree = getMaxOfTreeBy miniMaxABWithSeed

getMaxOfTreeBy:: (Board -> Int -> Int) -> Board -> Int
getMaxOfTreeBy mmFunc board = indexOfMax $ parMap rdeepseq (\(BoardColumn b i) -> (i, mmFunc b maxDepth)) $ computePossibleBoardStatesAfterTurn Player board
	where indexOfMax xs = fst $ foldl1 (\test1@(i1,heuristic1) test2@(i2,heuristic2) -> if heuristic1 > heuristic2 then test1 else test2) xs


getMaxAB::Board -> Int
getMaxAB board = fstThree $ foldl (\cur@(_,alpha,beta) (BoardColumn b i) -> let mmAB = runEval $ rpar$ miniMaxAB b maxDepth alpha beta True in if mmAB > alpha then (i,mmAB,beta) else cur) (0,minBound,maxBound) $ computePossibleBoardStatesAfterTurn Player board
	where indexOfMax xs = fst $ foldl1 (\test1@(i1,heuristic1) test2@(i2,heuristic2) -> if heuristic1 > heuristic2 then test1 else test2) xs


--This should start on the opponents turn because you consider the best of your opponents next turns
miniMaxPlayer board depth = miniMax board depth False

miniMax board depth isMaximizer 
	| (depth == 0) = getHeuristicOfBoard board
	| (hasWon board) = 100 - depth
	| (hasLost board) = depth - 100
	| (isMaximizer) =  maximum  $ map (\b -> miniMax b (depth - 1) False) nextPlayerBoards
	| (not isMaximizer) = minimum $ map (\b -> miniMax b (depth - 1) True) nextOpponentBoards
	where 	nextPlayerBoards = map bcBoard $computePossibleBoardStatesAfterTurn Player board
		nextOpponentBoards = map bcBoard $computePossibleBoardStatesAfterTurn Opponent board		


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


			nextPlayerBoards = map bcBoard $computePossibleBoardStatesAfterTurn Player board
			nextOpponentBoards = map bcBoard $computePossibleBoardStatesAfterTurn Opponent board		


fstThree (x,_,_) = x 

data GameTree = Node BoardColumn [GameTree] deriving (Show)

generateTree:: BoardColumn -> Int -> Piece -> GameTree
generateTree boardColumn depth startingPerson = generateTree' (Node boardColumn []) depth startingPerson
	where 	generateTree' curTree 0 _ = curTree
		generateTree' (Node boardcolumn@(BoardColumn board _) []) depth curPerson = Node boardcolumn (map (\b->generateTree' (Node b []) (depth - 1) (opposer curPerson)) $ computePossibleBoardStatesAfterTurn curPerson board)

miniMaxOnTree _ (Node (BoardColumn board _) []) = getHeuristicOfBoard board
miniMaxOnTree isMaximiser (Node (BoardColumn board _ ) xs) 
	| (hasWon board) = maxBound
	| (hasLost board) = minBound
	| (isMaximiser) = maximum $parMap rdeepseq (miniMaxOnTree (not isMaximiser)) xs
	| (not isMaximiser) = minimum $ parMap rdeepseq (miniMaxOnTree (not isMaximiser)) xs

newMiniMax board = maximum $ mapAndSeq (miniMaxOnTree True) $ map  (\b -> generateTree b maxDepth Opponent) $ computePossibleBoardStatesAfterTurn Player board


mapAndSeq = parMap rdeepseq

