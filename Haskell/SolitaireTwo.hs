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

  {-For each head of column, check if can be moved into foundations,if can add result to EOBoard
  (take stuff from first assignment)
  Do the same for reserves (take function from first assignemnt)
  Also have EOBoards of moving each head to reserves
  Go through reserves, then go through column heads, if can put it there add to that (use ass1
  for getting heads and using reserves - map/list comprehension)-}

  removeFromReserves::EOBoard -> [EOBoard]
  removeFromReserves board = [] ++ (newF, c, (removeFromReservesA newF r))
    where (f,c,r) = board --MIGHT NOT NEED AUX
          newF = getFoundations f r

  --Recurses through the foundations, removing from reserves if the card (and predecessors) exist in foundations
  removeFromReservesA::[Card]->[Card]->[Card]
  removeFromReservesA newF [] = [] --CHANGE AROUND
  removeFromReservesA [] r = r
  removeFromReservesA (h:t) r = removeFromReservesA t (filter (\x -> compare x h == GT) r)

  getFoundations::Foundations -> [Card] -> Foundations
  getFoundations f [] = f --FOR RESERVES
  getFoundations f (h:t)
    |isAce h = getFoundations (h:f) t
    |otherwise = getFoundations (map (\x -> (if x == pCard h then h else x)) f) t

  --list comprehension - take x from reserves,if can move it make the newF and take out of reserves,
  --and add that EOBoard to list of moves
{-reserves columns - for each reserve, check all heads and if can put it there,  add to list of EOBoards-}


  resToColumns::EOBoard->[EOBoard]->[EOBoard]
  resToColumns board newList
    | -- if new board with move same as original, return board newList
    | --tail-head recursion going through reserves, check if can move to any column
    |map (\res -> )
    where (f,c,(hr:tr)) = board
    columnHeads = [head n|n<-c, not(null n)]

  {-for columns
    for each head, check if can move to foundations and return that EOBoard
    return EOBoard of moving it to reserves
    }
