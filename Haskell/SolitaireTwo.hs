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



  resToColumns::EOBoard->[EOBoard] --IT WORKS
  resToColumns board@(f,c,r) = [resToColumnsA board res|res<-r, not(null res)]

  resToColumnsA::EOBoard->Card->EOBoard
  resToColumnsA board@(f,c,r) card
    |board==newBoard = ([],[[]],[]) --maybe change so in findMoves filters all boards that are same as original, or use Maybe andNothing
    |otherwise = newBoard
    where newC = (map (\col -> if sCard card == head col then card:col else col) c)
          cHeads = [head n|n<-newC, not(null n)]
          newBoard = (f,newC,(filter (\res -> (not(elem res cHeads))) r))

  kingToEmpty::EOBoard->[EOBoard]
  kingToEmpty board@(f,c,r)
    |filter (\n -> null n) c == [] = [([],[[]],[])] --check if there is an empty column
    |otherwise = resToEmpty board
    where cHeads = [head n|n<-c, not(null n)]

    --Do same but just for kings (move to empty column)
    --If king in reserves, move to empty column
    --If king in column, get head and move it

  resToEmpty::EOBoard->[EOBoard]
  resToEmpty board@(f,c,r) = [(f,newC card,newR card)|card<-kingCards]
    where kingCards = filter (\res -> isKing res) r
          newR card = filter (\res -> res /= card) r
          newC card = [if col==[] then card:col else col|col<-c,not(kingExists col card)]

  kingExists::Deck->Card->Bool
  kingExists column king
    |null column = False
    |((head column) == king) = True
    |otherwise = False

  colToReserves::EOBoard->[EOBoard]
  colToReserves board@(f,c,r)
    |length r >= 8 = [([],[[]],[])]
    |otherwise = [colToReservesA board (head col)|col<-c, not(null col)]

  colToReservesA::EOBoard->Card->EOBoard
  colToReservesA board@(f,c,r) card = newBoard
    -- |board==newBoard = ([],[[]],[])
    -- |otherwise = newBoard
    where newBoard = (f,map (\col -> if (head col == card) then (tail col) else col) c,(card:r))

  {-colToReserves::EOBoard->[EOBoard]->[EOBoard]
  colToReserves board@(f,c,r) list
    |length r > 8 = []
    |length r > 5 = [] --if reserves are too long, don't move it maybe?
    |otherwise = list ++ (map (\col -> makeColReserves (head col) board) c)

  makeColReserves::Card->EOBoard->EOBoard
  makeColReserves card board@(f,c,r) = (f,map (\n -> (if not(elem (head n) r) then n else (tail n)) c),filter (\n -> not(elem cHeads)))
    where cHeads = [head n|n<-c, not(null n)] --its all wrong sigh-}

  {- for each column, have eoboard to put in reserves
  -}

  {- for columns
    for each head, check if can move to foundations and return that EOBoard
    return EOBoard of moving it to reserves
  -}
