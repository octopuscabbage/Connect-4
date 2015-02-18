module BoardTest where

import Board
import Test.Hspec

boardTests = testWinning 


testWinning = describe "make sure win detection works" $ do 
	it "test columns work correct" $  (hasWon $ dropPlayer 6$dropPlayer 6 $ dropPlayer 6$dropPlayer 6 $ blankBoard) `shouldBe` True
	it "test rows work correctly" $ (hasWon $ dropPlayer 1$dropPlayer 2 $ dropPlayer 3$dropPlayer 4 $ blankBoard) `shouldBe` True
