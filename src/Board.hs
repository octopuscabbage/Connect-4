module Board where

import Data.Matrix
import qualified Data.Vector as V


data Piece = Player | Opponent | Empty deriving (Eq,Show,Read)

-- |Board addressing is always row column
type Board = Matrix Piece

-- |Constructs a blank board
blankBoard :: Board
blankBoard = matrix 6 7 $ const Empty

-- |Detect if you won
hasWon ::  Board -> Bool
hasWon = detectWin Player

-- |Detect if you've lost
hasLost ::  Board -> Bool
hasLost = detectWin Opponent

-- |Get a list of the rows of the board
getRows:: Board -> V.Vector (V.Vector Piece)
getRows board = V.map (\i-> getRow i board) $ V.fromList [1..6]

-- |Get a list of the columns of the board
getColumns:: Board -> V.Vector (V.Vector Piece)
getColumns board = V.map (\i-> getCol i board) $ V.fromList [1..7]

-- |Detect if player has won
detectWin:: Piece -> Board  ->  Bool
detectWin player board  = checkAnyVectorContainsWin (getRows board) || checkAnyVectorContainsWin (getColumns board) 
	where  	checkLinearWinCondition vec = V.foldl (\i element-> if i >= 4 then i else if element == player then i+1 else 1) 1 vec >= (4::Int) --The four isn't a magic number, this is connect four
		anyTrue = V.any (==True)	
		checkAnyVectorContainsWin vecs = anyTrue (V.map checkLinearWinCondition vecs) 



-- |Drops a piece in the specified column
dropPiece:: Piece -> Int -> Board -> Board
dropPiece piece column board = setElem piece (findLastEmptyRow, column) board
	where 	findLastEmptyRow = V.foldl (\i element -> if element == Empty then i+1 else i) 0 getColumn 
		getColumn = getCol column board

-- |Drop a piece on the player side
dropPlayer ::  Int -> Board -> Board
dropPlayer = dropPiece Player

-- |Drop a piece on the Opponent side
dropOpponent ::  Int -> Board -> Board
dropOpponent = dropPiece Opponent
