module Game where


import Board
import Data.Maybe
import Data.Functor
import System.TimeIt


watchTwoBotsPlayFromStart bot1F bot2F = watchTwoBotsPlayFromBoard bot1F bot2F blankBoard
watchTwoBotsPlayFromBoard bot1F bot2F board = do
	let boardAfterPlayer1Turn = fromJust $ dropPlayer (bot1F board) board
	timeIt $ print boardAfterPlayer1Turn
	if isFull boardAfterPlayer1Turn then print "bots drew, boring" >> return 0	
		else if detectWin Player boardAfterPlayer1Turn then print "player1 won" >> return 0
			else do
				let boardAfterPlayer2Turn = fromJust $ dropOpponent (bot2F $ flipPlayer boardAfterPlayer1Turn) boardAfterPlayer1Turn
				timeIt $ print boardAfterPlayer2Turn
				if isFull boardAfterPlayer2Turn then print "bots drew boring" >> return 0	
					else if detectWin Opponent boardAfterPlayer2Turn then print "Player 2 won" >> return 0
						else watchTwoBotsPlayFromBoard bot1F bot2F boardAfterPlayer2Turn

playPlayer opponentF = playPlayerFromBoard opponentF blankBoard 
playPlayerFromBoard opponentF board = do
	print board
	print " 1 2 3 4 5 6 7"
	print "What is your move? >> "
	playerMove <- (read <$> getLine)
	let boardAfterPlayerTurn = fromJust $dropPlayer playerMove board
	if isFull boardAfterPlayerTurn then print "IT'S A DRAAAAAWWWW!"	>> return 0
		else if detectWin Player boardAfterPlayerTurn then print "Player wins!Worthless AI, this is why you haven't replaced mcdonalds workers" >> return 0
			else do
				let flippedBoard = flipPlayer boardAfterPlayerTurn
				opponentMove <- opponentF flippedBoard
				timeIt $ print ("Computer plays " ++  show opponentMove)
				let boardAfterOpponentTurn = fromJust $ dropOpponent opponentMove boardAfterPlayerTurn
				if isFull boardAfterOpponentTurn then print "AI Made a draw, the only winning move is not to play" >> return 0
					else if detectWin Opponent boardAfterOpponentTurn then print boardAfterOpponentTurn >> print "The AI won, Skynet is coming" >> return 0
						else playPlayerFromBoard opponentF boardAfterOpponentTurn

