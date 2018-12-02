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
  Do the same for reserves (take function from first assignment)
  Also have EOBoards of moving each head to reserves
  Go through reserves, then go through column heads, if can put it there add to that (use ass1
  for getting heads and using reserves - map/list comprehension)-}

  --list comprehension - take x from reserves,if can move it make the newF and take out of reserves,
  --and add that EOBoard to list of moves
{-reserves columns - for each reserve, check all heads and if can put it there,  add to list of EOBoards-}


  resToColumns::EOBoard->[EOBoard]
  resToColumns board = resToColumnsA board r []
    where (f,c,r) = board

  --resToColumnsA::EOBoard->Deck->[EOBoard]->[EOBoard]
  --resToColumnsA board@(f,c,r) res newBoard = resToColumnsA board (tail res) (newBoard ++ (f, (map (\col -> (if ((pCard (head col))==n) then (n:col) else col) c)), [(filter (\n -> not (elem n (head col))) r)|col<-c]))

  resToColumnsB::EOBoard->Deck
  resToColumnsB board@(f,c,r) = map (\col -> (head col) c)

  colToReserves::EOBoard->[EOBoard]->[EOBoard]
  colToReserves board@(f,c,r) list
    |length r > 8 = []
    |length r > 5 = [] --if reserves are too long, don't move it maybe?
    |otherwise = list ++ (map (\col -> makeColReserves (head col) board) c)

  makeColReserves::Card->EOBoard->EOBoard
  makeColReserves card board@(f,c,r) = (f,map (\n -> (if not(elem (head n) r) then n else (tail n)) c)     ,filter (\n -> not(elem cHeads)))
    where cHeads = [head n|n<-c, not(null n)] --its all wrong sigh
          updatedC =

  {- for each column, have eoboard to put in reserves
  -}



  {-resToColumnsA::EOBoard->Deck->[EOBoard]->[EOBoard]
  resToColumnsA board (hr:tr) posMoves = posMoves
  --  |  if new board with move same as original, return board newList
  --  | tail-head recursion going through reserves, check if can move to any column
  --  |map (\col -> )
    where (f,c,r) = board
          columnHeads = [head n|n<-c, not(null n)] --naaaaaahhhhhh
          newF =-}

  {- for columns
    for each head, check if can move to foundations and return that EOBoard
    return EOBoard of moving it to reserves
  -}
