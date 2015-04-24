module Main where



import Game
import AI
import Board
import AIRedux

main = do
	print "Which Player Am I [1/2]"
	player <- getLine 
	let playerNum = ((read player) :: Int)
	if (playerNum == 2)
          then playPlayer (return . treeBotRedux)
          else playOpponent (return .treeBotRedux)
	return ()	

compareBots =  watchTwoBotsPlayFromStart treeBotRedux treeBotRedux


playMe  bot = playPlayer (return . bot)

treeBot board = getMaxOfTree board

treeBot2 board = newMiniMax board

treeBotRedux = computeNextMove

threadScope func = func blankBoard
