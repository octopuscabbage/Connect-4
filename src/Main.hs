module Main where


import Game
import AI
import Control.Applicative


main = runGameAndWriteOut maxBot maxBot

maxBot board = fst <$> getMaxNextStateAndIndex board
	
