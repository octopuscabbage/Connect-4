module AI where

import Data.HashTable.Class
import qualified Data.HashTAble.IO as h
import Board


-- |A Hashtable of possible board states and their current weights
type StateWeights = H.BasicHashTable Board Int




computePossibleBoardStatesAfterTurn:: Piece -> Board -> [Board]
computePossibleBoardStatesAfterTurn player currentboard = map (\i->dropPiece player  i currentboard) [1..7]


