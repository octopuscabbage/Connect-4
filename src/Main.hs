module Main where



import Game
import AI
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Heuristics

main = runGameAndWriteOut playerBot maxBot

maxBot board = fst <$> getMaxNextStateAndIndex board

heuristicBot board = getNextMoveByHeuristic board
	
playMaxBots = runGameAndWriteOut maxBot maxBot

playForever = forever playMaxBots

playManyForever =forkIO playForever >> playForever


playerBot board = print board >> print "What is your move? 1 is right, 7 is left>> " >> getLine >>= return . read
