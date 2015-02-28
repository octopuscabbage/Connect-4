module Main where



import Game
import AI
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Heuristics

main = compareBots

compareBots =  watchTwoBotsPlayFromStart treeBot treeBot


playMe = playPlayer (return . treeBot)

treeBot board = getMaxOfTree board

