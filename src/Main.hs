module Main where



import Game
import AI
import Board
import AIRedux

main = compareBots 

compareBots =  watchTwoBotsPlayFromStart treeBotRedux treeBotRedux


playMe  bot = playPlayer (return . bot)

treeBot board = getMaxOfTree board

treeBot2 board = newMiniMax board

treeBotRedux = computeNextMove

threadScope func = func blankBoard
