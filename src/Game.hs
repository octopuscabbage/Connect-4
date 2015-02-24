module Game where


import Board
import Control.Monad.Writer
import AIDB
import Data.Maybe

data Result = Win | Loss deriving (Eq, Show)

-- | PlayerBoard, OpponentBoard and IO piece that won
type Game = WriterT [(Piece,Board)] IO Piece


runGame:: (Board -> IO Int) -> (Board -> IO Int) -> Game	
runGame playerF opponentF = runGameFromBoard playerF opponentF blankBoard

--Todo flip boards so that function sees itself as player
runGameFromBoard:: (Board -> IO Int) -> (Board -> IO Int) -> Board -> Game	
runGameFromBoard playerF opponentF board = do
	playerMove <- lift $ playerF board
	let boardAfterPlayerTurn = fromJust $ dropPlayer playerMove board
	tell [(Player,boardAfterPlayerTurn)]
	if isFull boardAfterPlayerTurn then return Empty 
	else 
		if detectWin Player boardAfterPlayerTurn
			then return Player
			else do
				let flippedBoard = flipPlayer boardAfterPlayerTurn
				opponentMove <- lift $ opponentF flippedBoard
				let boardAfterOpponentTurn = fromJust $ dropOpponent opponentMove $ boardAfterPlayerTurn
				tell [(Opponent,flipPlayer boardAfterOpponentTurn)]
				if isFull boardAfterOpponentTurn then return Empty 
					else if detectWin Opponent boardAfterOpponentTurn then return Opponent
						else runGameFromBoard playerF opponentF boardAfterOpponentTurn
		


runGameAndWriteOut:: (Board -> IO Int) -> (Board -> IO Int) -> IO ()
runGameAndWriteOut playerF opponentF = do
	(result,log) <- runWriterT $ runGame playerF opponentF
	if result == Empty then do
		writeLoss Player log --if you're not first you're last
		writeLoss Opponent log
		else do
		writeWin result log
		writeLoss (opposer result) log
		return ()
		

writeWin piece log = updateWin $ map snd $ filter (\(p,s) -> p == piece) log
writeLoss piece log = updateLoss $ map snd $ filter (\(p,s) -> p == piece) log


