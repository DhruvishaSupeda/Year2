module SolitaireTwo where

  import System.Random
  import Data.List
  import Data.List.Split
  import SolitaireOne
  import Data.Maybe

----------------------------------------------------------------------------------------

  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board = Just board
  --  |isAce head h =
--  where (f,c,r) = board
  --      (h:t) = c

  findMoves :: EOBoard -> [EOBoard]
  findMoves board = [board,board]
