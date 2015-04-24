module AIRedux where

import Board
import Heuristics
import Control.Parallel.Strategies

maxDepth:: Int
maxDepth = 7

data ColumnWeight = ColumnWeight{
  getWeight::Int,
  getColumn:: Int
 } deriving (Eq)

instance Ord ColumnWeight where
  (ColumnWeight w _ ) `compare` (ColumnWeight w2 _) = w `compare` w2

computeNextMove::Board -> Int
computeNextMove board
  | (board == blankBoard) = 4
  | otherwise = getColumn $ maximum $ map (\(BoardColumn b c) -> ColumnWeight(minMax b maxDepth False ) c )nextPlayerBoards

                        where nextPlayerBoards =computePossibleBoardStatesAfterTurn Player board
                              nextOpponentBoards = computePossibleBoardStatesAfterTurn Opponent board
mapSeq = parMap rseq

mapAndDSeqp = parMap rdeepseq

minMax:: Board -> Int -> Bool -> Int
minMax board 0 _ = getHeuristicOfBoard board --Might need to negate if winner or loser
minMax board depth maximizingPlayer
  | null nextPlayerBoards || null nextOpponentBoards = 0
  | isFull board = 0
  | hasWon board = 1000 - depth
  | hasLost board = depth - 1000
  | maximizingPlayer = maximum  $ minmaxchildren  nextPlayerBoards
  | not maximizingPlayer = minimum $ minmaxchildren nextOpponentBoards
                       where nextPlayerBoards = map bcBoard $ computePossibleBoardStatesAfterTurn Player board
                             nextOpponentBoards = map bcBoard $ computePossibleBoardStatesAfterTurn Opponent board
                             minmaxchildren = mapAndDSeqp (\ b-> minMax b (depth - 1) (not maximizingPlayer))
