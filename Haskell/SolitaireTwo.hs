module SolitaireTwo where

  import System.Random
  import Data.List
  import Data.List.Split
  import SolitaireOne
  --import Solitaire1PDG
  import Data.Maybe

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

  eOExpt::Float
  eOExpt = (fromIntegral (foldr (+) 0 (map (\random -> eOGame (eODeal random)) random))) / 100
    where random = take 100 (randoms (mkStdGen 42)::[Int])

  eOGame::EOBoard->Int
  eOGame board = eOGameA board 0

  eOGameA::EOBoard->Int->Int
  eOGameA board score
    |isNothing (chooseMove board) = score
    |otherwise = eOGameA (fromJust (chooseMove board)) (score+1)

  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board
    |null (findMoves board)  = Nothing
    |otherwise = Just (head (findMoves board)) --choosing justhead means endlessloop if one move left

  findMoves :: EOBoard -> [EOBoard]
  findMoves board = [toFoundations board|board<-newBoards, board/=([],[[]],[])]
    where newBoards = resToColumns board++colToColumns board++kingToEmpty board -- ++colToReserves board

  getLengths :: [EOBoard] -> [(EOBoard,Int)]
  getLengths boards = zip boards weights
    where weights = [cValues + length r|board@(f,c,r)<-boards]
          columns = [ c|board@(f,c,r)<-boards]
          cValues = foldr (+) 0 [length col|col<-columns]

  hello::EOBoard
  hello = ([],[[(Two,Spades),(Three,Spades)],[(Four,Spades)],[(Five,Spades)],[(Ace,Diamonds)],[],[],[],[]],[])

  order::EOBoard
  order = ([], chunksOf 6 (drop 4 pack), (take 4 pack))

  won::EOBoard
  won = ([(King,Spades),(King,Clubs),(King,Diamonds),(King,Hearts)],[[],[],[],[],[],[],[],[]],[])

----------------------------------------------------------------------------------------------------

  resToColumns::EOBoard->[EOBoard]
  resToColumns board@(f,c,r) = [resToColumnsA board res|res<-r, not(null res)]

  resToColumnsA::EOBoard->Card->EOBoard
  resToColumnsA board@(f,c,r) card
    |board==newBoard = ([],[[]],[])
    |otherwise = newBoard
    where newC = [(if (not(isKing card) && (not(null col)) && (sCard card == head col)) then card:col else col)|col<-c]
        --  newC = (map (\col -> if (not(isKing card) && sCard card == head col) then card:col else col) c)
          cHeads = [head n|n<-newC, not(null n)]
          newBoard = (f,newC,(filter (\res -> (not(elem res cHeads))) r))

------------------------------------------------------------------------------------------------------

  kingToEmpty::EOBoard->[EOBoard]
  kingToEmpty board@(f,c,r)
    |filter (\n -> null n) c == [] = [([],[[]],[])] --check if there is an empty column
    |otherwise = colToEmpty board ++ resToEmpty board

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

  colToReserves::EOBoard->[EOBoard]
  colToReserves board@(f,c,r)
    |length r >= 8 = [([],[[]],[])]
    |otherwise = [colToReservesA board (head col)|col<-c, not(null col)]

  colToReservesA::EOBoard->Card->EOBoard
  colToReservesA board@(f,c,r) card
    |board==newBoard = ([],[[]],[])
    |null c = ([],[[]],[])
    |otherwise = newBoard
    where newBoard = (f,map (\col -> if (not(null col)&&(head col == card)) then (tail col) else col) c,(card:r))

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
    |null tc = False
    |not(isKing (last stack)) && (h == sCard (last stack)) = True
    |otherwise = canBeMoved tc stack
