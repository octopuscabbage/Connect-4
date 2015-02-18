module Board where

import Data.Matrix
import qualified Data.Vector as V

data Piece = Player | Opponent | Empty deriving (Eq,Show,Read)
type Board = Matrix Piece

-- |Board addressing is always row column

-- |Constructs a blank board
blankBoard :: Board
blankBoard = matrix 6 7 (\_ -> Empty) 

-- |Detect if player has won
detectWin:: Board  -> Piece-> Bool
detectWin board player= undefined


-- |Drops a piece in the specified column
dropPiece:: Piece -> Int -> Board -> Board
dropPiece piece column board = setElem piece (findLastEmptyRow, column) board
	where 	findLastEmptyRow = V.foldl (\i element -> if element == Empty then i+1 else i) 0  $ getColumn 
		getColumn = getCol column board

dropPlayer ::  Int -> Board -> Board
dropPlayer = dropPiece Player

dropOpponent ::  Int -> Board -> Board
dropOpponent = dropPiece Opponent


