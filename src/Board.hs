module Board where

import Data.Matrix
import qualified Data.Vector as V

data Piece = Player | Opponent | Empty deriving (Eq,Show,Read)

opposer Player = Opponent
opposer Opponent = Player

nonEmpty Empty = False
nonEmpty _ = True

minShow Player = "P"
minShow Opponent = "O"
minShow Empty = "E"

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
-- |TODO: Detect diagnal wins
{--
detectWin:: Piece -> Board  ->  Bool
detectWin player board  = checkAnyVectorContainsWin (getRows board) || checkAnyVectorContainsWin (getColumns board) 
	where  	checkLinearWinCondition vec = V.foldl (\i element-> if i >= 5 then i else if element == player then i+1 else 1) 1 vec >= (5::Int) --The four isn't a magic number, this is connect four
		anyTrue = V.any (==True)	
		checkAnyVectorContainsWin vecs = anyTrue (V.map checkLinearWinCondition vecs) 

--}

detectWin:: Piece -> Board -> Bool
detectWin player board = any (>=4) $ getAllRuns player board 

getAllRuns player board = concatMap (getRuns player board) $ filter (\(BoardPos p _ _) -> p == player) $ toListOfBoardPos board
-- | Given a player and a board and an initial boardpos what's the distances you can go from it while still being in a 'run'
-- | a run is defined as consective piece of the same value
getRuns:: Piece -> Board -> BoardPos -> [Int]
getRuns player board boardPos = map ($ boardPos) [getLine (\r c -> (r+1,c)), getLine (\r c -> (r - 1,c)), getLine (\r c -> (r,c+1)),getLine (\r c ->( r, c-1)), getLine (\r c -> (r+1,c+1)), getLine (\r c -> (r+1,c-1)), getLine (\r c -> (r-1,c+1)), getLine (\r c -> (r-1,c-1))]
		where getLine movF boardPos = getLine' 0 boardPos
			where getLine' n (BoardPos piece row column)
				| (not $ isInsideMatrix row column) = n
				| (piece /= player) = n
				| otherwise = getLine' (n+1) $ posToBoardPos board nextRow nextColumn
					where 	nextPos = movF row column
						nextRow = fst nextPos	
						nextColumn = snd nextPos
				
posToBoardPos board row column = BoardPos (board ! (row,column)) row column

isInsideMatrix r c = (r > 0 && c > 0 && r <= 6 && c <= 7)

-- |Drops a piece in the specified column
dropPiece:: Piece -> Int -> Board -> Maybe Board
dropPiece piece column board = if columnContainsEmptyRow then Just $ setElem piece (findLastEmptyRow, column) board else Nothing
	where 	findLastEmptyRow = V.foldl (\i element -> if element == Empty then i+1 else i) 0 getColumn 
		getColumn = getCol column board
		columnContainsEmptyRow = V.any (==Empty) getColumn

-- |Drop a piece on the player side
dropPlayer ::  Int -> Board -> Maybe Board
dropPlayer = dropPiece Player

-- |Drop a piece on the Opponent side
dropOpponent ::  Int -> Board -> Maybe Board
dropOpponent = dropPiece Opponent

flipPlayer board = fmap flipPiece board
	where 	flipPiece Player = Opponent
		flipPiece Opponent = Player	
		flipPiece Empty = Empty

getAllPositions = sequence [[1..6],[1..7]]

toListOfBoardPos:: Board -> [BoardPos]
toListOfBoardPos board = map (\(r:c:[]) -> BoardPos (board ! (r,c)) r c) getAllPositions

minString:: Board -> String
minString board= concatMap show $ filter (\(BoardPos p _ _) -> nonEmpty p) $ toListOfBoardPos board

isFull:: Board -> Bool
isFull board = not $ V.any (==True) $ V.map (\v -> V.any (==Empty) v)  $ getRows board

-- | Boardpos is a piece followed by it's row and column
data BoardPos = BoardPos Piece Int Int

instance Show BoardPos where 
	show (BoardPos Empty row column) = ""
	show (BoardPos Player row column) = "P"++" "++(show row)++" "++(show column)
	show (BoardPos Opponent row column) = "O"++" "++(show row) ++ " " ++ show column 
