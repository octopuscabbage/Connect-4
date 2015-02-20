module AI where

import Board
import AIDB


-- |Represents a state and it's associated weight
data StateWeight = StateWeight { stateBoard:: Board,  stateValue::Int} deriving (Eq, Show)

instance Ord StateWeight where
	(StateWeight _ v1) `compare` (StateWeight _ v2) =  v1 `compare` v2

getStateWeight:: Board -> IO StateWeight
getStateWeight board= getValue (show board) >>=  return . StateWeight board . maybeToValue
	where 	maybeToValue (Just x) = x
		maybeToValue Nothing = 0	

findStateWeightsForNextTurn:: Piece -> Board -> IO [StateWeight]
findStateWeightsForNextTurn player currentBoard = mapM getStateWeight $ computePossibleBoardStatesAfterTurn player currentBoard

getMaxNextStateAndIndex:: Board -> IO (Int,StateWeight)
getMaxNextStateAndIndex board = findStateWeightsForNextTurn Player board >>= return .  maximumWithIndex . zip [1..7]
	where maximumWithIndex = foldl (\cur@(_,curState) test@(_,testState) -> if testState>curState then test else cur) (0,StateWeight (blankBoard) (minBound::Int))


-- |Returns all the board states possible from the current board state
computePossibleBoardStatesAfterTurn:: Piece -> Board -> [Board]
computePossibleBoardStatesAfterTurn player currentboard = map (\i->dropPiece player  i currentboard) [1..7]

