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

  hello::EOBoard
  hello = ([],[[(Two,Spades),(Three,Spades)],[(Four,Spades)],[],[],[],[],[],[]],[])

----------------------------------------------------------------------------------------------------

  resToColumns::EOBoard->[EOBoard] --IT WORKS
  resToColumns board@(f,c,r) = [resToColumnsA board res|res<-r, not(null res)]

  resToColumnsA::EOBoard->Card->EOBoard
  resToColumnsA board@(f,c,r) card
    |board==newBoard = ([],[[]],[]) --maybe change so in findMoves filters all boards that are same as original, or use Maybe andNothing
    |otherwise = newBoard
    where newC = (map (\col -> if sCard card == head col then card:col else col) c)
          cHeads = [head n|n<-newC, not(null n)]
          newBoard = (f,newC,(filter (\res -> (not(elem res cHeads))) r))

------------------------------------------------------------------------------------------------------

  kingToEmpty::EOBoard->[EOBoard]
  kingToEmpty board@(f,c,r)
    |filter (\n -> null n) c == [] = [([],[[]],[])] --check if there is an empty column
    |otherwise = colToEmpty board ++ resToEmpty board

  resToEmpty::EOBoard->[EOBoard]
  resToEmpty board@(f,c,r) = [(f,kingNewC c card,newR card)|card<-kingCards]
    where kingCards = filter (\res -> isKing res) r
          newR card = filter (\res -> res /= card) r

  kingNewC::Columns->Card->Columns
  kingNewC columns@(hc:tc) king
    |null hc = (king:hc):tc
    |otherwise = hc:kingNewC tc king

  colToEmpty::EOBoard->[EOBoard]
  colToEmpty board@(f,c,r) = [(f,kingNewC (newC card) card,r)|card <- kingCards]
    where cHeads = [head n|n<-c, not(null n)]
          kingCards = filter (\card -> isKing card) cHeads
          newC card = map (\col -> if (not(null col)&&(head col == card)) then (tail col) else col) c

------------------------------------------------------------------------------------------------------

  colToReserves::EOBoard->[EOBoard]
  colToReserves board@(f,c,r)
    |length r >= 8 = [([],[[]],[])]
    |otherwise = [colToReservesA board (head col)|col<-c, not(null col)]

  colToReservesA::EOBoard->Card->EOBoard
  colToReservesA board@(f,c,r) card
    |board==newBoard = ([],[[]],[])
    |otherwise = newBoard
    where newBoard = (f,map (\col -> if (head col == card) then (tail col) else col) c,(card:r))

-----------------------------------------------------------------------------------------------------

  colToColumns::EOBoard->[EOBoard]
  colToColumns board@(f,c,r) = [(f,(cNewC stack c board),r)|stack<-stacks, (f,(cNewC stack c board),r)/=board]
    where stacks = [getStack col []|col<-c]

  getStack::Deck->Deck->Deck
  getStack [] _ = []
  getStack col@(h:t) stack
    |null stack = getStack t [h]
    |(last stack == pCard h) = getStack t stack++[h]
    |otherwise = stack

  cNewC::Deck->Columns->EOBoard->Columns
  cNewC stack c board= [getNewColumn stack col board|col<-c]

  getNewColumn::Deck->Deck->EOBoard->Deck
  getNewColumn stack col board@(f,c,r)
    |null col = col
    |null stack = col
    |head col == sCard (last stack) = stack++col
    |(isInfixOf stack col) && (canBeMoved c stack) = col \\ stack
    |otherwise = col

  canBeMoved::Columns->Deck->Bool
  canBeMoved _ [] = False
  canBeMoved [] _ = False
  canBeMoved columns@(h:t) stack
    |head h == sCard (last stack) = True
    |otherwise = canBeMoved t stack
