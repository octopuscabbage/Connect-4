module AITypes where
import Board

-- |Represents a state and it's associated weight
data StateWeight = StateWeight { stateBoard:: Board,  stateHeuristic::Int} deriving (Eq, Show)

instance Ord StateWeight where
	(StateWeight _ v1) `compare` (StateWeight _ v2) =  v1 `compare` v2


