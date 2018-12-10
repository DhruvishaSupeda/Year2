module SolitaireTwo where

  import System.Random
  import Data.List
  import Data.List.Split
  import SolitaireOne
  --import Solitaire1PDG
  import Data.Maybe

  --if one move in findMoves, stop? or if it is colToReserves and the card tomove has sCard in same column?

----------------------------------------------------------------------------------------
  shuffle::Int->Deck
  shuffle seed = map fst (sortBy (\(_,x) (_,y) -> compare x y) (zip pack (getInts seed)))

  --Function to get list of random ints
  getInts::Int->[Int]
  getInts seed = take 52 (randoms (mkStdGen seed)::[Int])

  --Splits the shuffled deck into a playable board
  eODeal::Int->EOBoard
  eODeal seed = ([], chunksOf 6 (drop 4 (shuffle seed)), (take 4 (shuffle seed)))

----------------------------------------------------------------------------------------
  --Plays 100 games, gives back average score and (not yet) no of games won
  eOExpt::Float
  eOExpt = (fromIntegral (foldr (+) 0 (map (\random -> eOGame (eODeal random)) random))) / 3
    where random = take 3 (randoms (mkStdGen 42)::[Int])

  eOGame::EOBoard->Int
  eOGame board = eOGameA board 0

  --Plays a game and returns a score
  eOGameA::EOBoard->Int->Int
  eOGameA board score
    |isNothing (chooseMove board) = score
    |otherwise = eOGameA (fromJust (chooseMove board)) (score+1) --score not correct

  --Chooses a move out of list of EOBoards from findMoves
  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board@(f,c,r)
    |null newBoards  = Nothing

    --SCORE THEM ALL, THEN USE HIGHEST SCORE INSTEAD OF HEAD
  --  |not (null kingAtHead) = head kingAtHead
    |(filter (\b -> b /= ([],[[]],[])) (kingToEmpty board)) /= [] = Just (head (kingToEmpty board))
    --if toFoundations onnewBoard is different, use that one
--    |diffToF /= [] = Just (head diffToF)
    --if need to do col to reserve,find res to col next go - infinite loop maybe?
    -- |[if (length nr>r)|board@(nf,nc,nr) <- newBoards] --god knows
    |otherwise = Just (head newBoards) --choosing justhead means endlessloop if one move leftr
    where newBoards = findMoves board
      --    kingAtHead = filter(\board -> checkColsForKing board) newBoards CHECK RESERVES AND COLUMNS
          diffToF = (filter (\b -> (toFoundations b) /= b) newBoards)

  diff::EOBoard->[EOBoard]
  diff board = (filter (\b -> (toFoundations b) /= b) newBoards)
    where newBoards = findMoves board

  checkForKing::EOBoard->Bool
  checkForKing board@(f,c,r)
    |filter (\res -> isKing res) r /= [] = True
    |filter (\col -> isKing col) cHeads /= [] = True
    |otherwise = False
    where cHeads = [head n|n<-c, not(null n)]


  --Finds possible moves using all functions
  --Function that if king exposed, and theres an empty column, choose that move
  --Function that if move is made then toFoundations on new board is different,  use taht move LIST COMPS?
  findMoves :: EOBoard -> [EOBoard]
  findMoves board = [toFoundations board|board<-newBoards, board/=([],[[]],[])]
    where newBoards = resToColumns board++colToColumns board++kingToEmpty board ++ stackToReserves board

  getLengths :: [EOBoard] -> [(EOBoard,Int)]
  getLengths boards = zip boards weights
    where weights = [cValues + length r|board@(f,c,r)<-boards]
          columns = [ c|board@(f,c,r)<-boards]
          cValues = foldr (+) 0 [length col|col<-columns]

  hello::EOBoard
  hello = ([],[[(Two,Spades),(Three,Spades)],[(Four,Spades)],[(Five,Spades)],[(Ace,Diamonds)],[],[],[],[]],[])

  bye::EOBoard
  bye = ([],[[(Two,Spades),(Three,Spades)],[(Four,Spades)],[(Five,Spades)],[(Ace,Diamonds)],[(King,Spades)],[],[],[]],[])

  order::EOBoard
  order = ([], chunksOf 6 (drop 4 pack), (take 4 pack))

  won::EOBoard
  won = ([(King,Spades),(King,Clubs),(King,Diamonds),(King,Hearts)],[[],[],[],[],[],[],[],[]],[])

----------------------------------------------------------------------------------------------------

--for all coltores, if any equal board, false else true

  resToColumns::EOBoard->[EOBoard]
  resToColumns board@(f,c,r) = [resToColumnsA board res|res<-r, not(null res)]

  resToColumnsA::EOBoard->Card->EOBoard
  resToColumnsA board@(f,c,r) card
    |board==newBoard = ([],[[]],[])
    |repeatedBoard board newBoard == True = ([],[[]],[])
    |otherwise = newBoard
    where newC = [(if (not(isKing card) && (not(null col)) && (sCard card == head col)) then card:col else col)|col<-c]
        --  newC = (map (\col -> if (not(isKing card) && sCard card == head col) then card:col else col) c)
          cHeads = [head n|n<-newC, not(null n)]
          newBoard = (f,newC,(filter (\res -> (not(elem res cHeads))) r))

  repeatedBoard::EOBoard->EOBoard->Bool
  repeatedBoard old new = if ((filter (\b -> b==old) (stackToReserves new)) /= []) then True else False

------------------------------------------------------------------------------------------------------

  kingToEmpty::EOBoard->[EOBoard]
  kingToEmpty board@(f,c,r)
    |filter (\n -> null n) c == [] = [([],[[]],[])] --check if there is an empty column
    |otherwise = resToEmpty board ++ colToEmpty board

  resToEmpty::EOBoard->[EOBoard]
  resToEmpty board@(f,c,r) = [(f,kingNewC c card,newR card)|card<-kingCards,r/=[]]
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

  {-colToReserves::EOBoard->[EOBoard]
  colToReserves board@(f,c,r)
    |length r >= 8 = [([],[[]],[])]
    |otherwise = [colToReservesA board (head col)|col<-c, not(null col)]

  colToReservesA::EOBoard->Card->EOBoard
  colToReservesA board@(f,c,r) card
    |board==newBoard = ([],[[]],[])
    |null c = ([],[[]],[])
    |otherwise = newBoard
    where newBoard = (f,map (\col -> if (not(null col)&&(head col == card)) then (tail col) else col) c,(card:r)) -}

-----------------------------------------------------------------------------------------------------

  stackToReserves::EOBoard->[EOBoard]
  stackToReserves board@(f,c,r)
    |(kingToEmpty board) /= [([],[[]],[])] && (kingToEmpty board) /= [] = [([],[[]],[])]
    |length r >= 8 = [([],[[]],[])]
    |otherwise = [(f,makeNewColumns c stack r,newReserves stack r)|stack<-stacks]
    where stacks = [getStack col []|col<-c, not(null col)]

  makeNewColumns::Columns->Deck->Deck->Columns
  makeNewColumns columns stack r = [newCol stack col r|col<-columns]

  newCol::Deck->Deck->Deck->Deck
  newCol _ [] _ = []
  newCol [] col _ = col
  newCol stack@(h:t) col r
    |(isInfixOf [h] col) && ((length r) <= 8) = newCol t (col\\([h])) (r++[h])
    |otherwise = col

  newReserves::Deck->Deck->Deck
  newReserves [] r = r
  newReserves stack@(h:t) reserves
    |length reserves < 8 = newReserves t (reserves++([h]))
    |otherwise = reserves

  --go through columns, get stack for column
  --go through stack, add head of stack to r if r < 8, then recurse with tail of stack
  --if >8,

-----------------------------------------------------------------------------------------------------

  colToColumns::EOBoard->[EOBoard]
  colToColumns board@(f,c,r) = [(f,(cNewC stack c board),r)|stack<-stacks, ((f,(cNewC stack c board),r)/=board)]
    where stacks = [getStack col []|col<-c]

  getStack::Deck->Deck->Deck
  getStack [] stack = stack
  getStack col@(h:t) [] = getStack t [h]
  getStack col@(h:t) stack
    |(not(isKing (last stack)) && ((sCard (last stack)) == h)) = getStack t (stack++[h])
    |otherwise = stack

  cNewC::Deck->Columns->EOBoard->Columns
  cNewC stack c board= [getNewColumn stack col board|col<-c]

  getNewColumn::Deck->Deck->EOBoard->Deck
  getNewColumn _ [] _ = []
  getNewColumn [] col _ = col
  --getNewColumn _ col null = col
  getNewColumn stack col@(hc:tc) board@(f,c,r)
    |(not(isKing (last stack)) && (hc == sCard (last stack))) = stack++col
    |(isInfixOf stack col) && (canBeMoved c stack) = col \\ stack
    |otherwise = col

  canBeMoved::Columns->Deck->Bool
  canBeMoved _ [] = False
  canBeMoved ([]:_) _ = False
  canBeMoved columns@((h:t):tc) stack
    |null columns = False
    |null tc && not (null (h:t)) = if (not(isKing (last stack)) && (h == sCard (last stack))) then True else False
    |not(isKing (last stack)) && (h == sCard (last stack)) = True
    |otherwise = canBeMoved tc stack
