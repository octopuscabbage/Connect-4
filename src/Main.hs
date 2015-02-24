module Main where



import Game
import AI
import Control.Applicative
import Control.Concurrent
import Control.Monad

main = playForever

maxBot board = fst <$> getMaxNextStateAndIndex board
	
playMaxBots = runGameAndWriteOut maxBot maxBot

playForever = forever playMaxBots

playManyForever =forkIO playForever >> playForever


playerBot board = print board >> print "What is your move? 1 is right, 7 is left>> " >> getLine >>= return . read
