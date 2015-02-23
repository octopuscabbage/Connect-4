module AI where

import Board
import AIDB
import AITypes
import System.Random
import Data.Maybe


getStateWeight:: Board -> IO StateWeight
getStateWeight board= getValue (show board) >>=  return . StateWeight board . maybeToValue
	where 	maybeToValue (Just x) = x
		maybeToValue Nothing = 0	

findStateWeightsForNextTurn:: Piece -> Board -> IO [(Int,StateWeight)]
findStateWeightsForNextTurn player currentBoard = mapM (\(i, b) -> getStateWeight b >>= \sw -> return (i,sw)) $ computePossibleBoardStatesAfterTurn player currentBoard

getMaxNextStateAndIndex:: Board -> IO (Int,StateWeight)
getMaxNextStateAndIndex board = do
		weights <- findStateWeightsForNextTurn Player board
		let max = findMax weights
		print max
		return max
	where findMax bs = foldl (\ cur@(_,curstate) test@(_,teststate) -> if curstate > teststate then cur else test) (0,StateWeight blankBoard (minBound::Int)) bs


-- |Returns all the board states possible from the current board state
computePossibleBoardStatesAfterTurn:: Piece -> Board -> [(Int,Board)]
computePossibleBoardStatesAfterTurn player currentboard =  fromJust $sequence$ filter isJust $ map (\i->dropPiece player i currentboard >>= (\next -> return (i,next))) [1..7]

getRandom:: IO Int
getRandom = randomRIO (1,7)
