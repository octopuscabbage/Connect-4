module BoardTest where

import Board
import Test.Hspec
import Data.Maybe

boardTests = testWinning 


testWinning = describe "make sure win detection works" $ do 
	it "test columns work correct" $  (hasWon $ unsafeDrop 6$unsafeDrop 6 $ unsafeDrop 6$unsafeDrop 6 $ blankBoard) `shouldBe` True
	it "test rows work correctly" $ (hasWon $ unsafeDrop 1$unsafeDrop 2 $ unsafeDrop 3$unsafeDrop 4 $ blankBoard) `shouldBe` True
	it "test rows does not work incorrectly" $ (hasWon $ unsafeDrop 1 $ blankBoard) `shouldBe` False
	it "tests columns does not work incorrectly" $ (hasWon $ unsafeDrop 1 $ unsafeDrop 1 $ blankBoard) `shouldBe` False
	it "test diags are working" $ (hasWon $ unsafeDropOpponent 1 $ unsafeDropOpponent 1 $ unsafeDropOpponent 1 $ unsafeDrop 1 $ unsafeDrop 2 $ unsafeDropOpponent 2 $ unsafeDrop 2 $ unsafeDropOpponent 3 $ unsafeDrop 3 $ unsafeDrop 4 $ blankBoard) `shouldBe` True
	
unsafeDrop pos board = fromJust $ dropPlayer pos board
unsafeDropOpponent pos board = fromJust $ dropOpponent pos board
