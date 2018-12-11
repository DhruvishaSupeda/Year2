module SolitaireTwo where

  import System.Random
  import Data.List
  import Data.List.Split
  import SolitaireOne
--  import Solitaire1PDG
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
  eOExpt::(Int,Float)
  eOExpt = ((length (filter (\s -> s==52) scores)),((fromIntegral (foldr (+) 0 scores) / 25)))
    where random = take 25 (randoms (mkStdGen 60)::[Int])
          scores = (map (\random -> eOGame (eODeal random)) random)

  --Plays a game and returns a score
  eOGame::EOBoard->Int
  eOGame board@(f,c,r)
    |isNothing (chooseMove board) = (52- (length r) - (foldr (+) 0 (map length c)))
  --  |not (boardsNotEqual (fromMaybe ([],[[]],[]) (chooseMoveStupid (fromJust (chooseMove board)))) board) = (52- (length r) - (foldr (+) 0 (map length c)))
    |otherwise = eOGame (fromJust (chooseMove board)) --score not correct

  --Chooses a move out of list of EOBoards from findMoves
  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board@(f,c,r)
    |null newBoards = Nothing
    --Moves a king if it can
    |(length newBoards == 1) = Nothing
  --  |(not (null (kToE))) = Just (head kToE)
    |(isJust (cardSecond board c)) && (secondCardsList /= []) = Just (head secondCardsList)
    --if toFoundations onnewBoard is different, use that one
    |diffToF /= [] = Just (head diffToF)
    --if need to do col to reserve,find res to col next go - infinite loop maybe?
--    |(not (null (rToC)))= Just (head rToC)
    |not (null (newBoards)) = Just (last (weightedBoards))
    |otherwise = Nothing --choosing justhead means endlessloop if one move left
    where newBoards = (filter (\b -> boardsNotEqual b board) (findMoves board))
          weightedBoards = map fst (sortBy (\(_,x) (_,y) -> compare x y) (map addWeights newBoards))
          diffToF = (filter (\b -> (boardsNotEqual (toFoundations b) b)) newBoards)
          index = cardSecond board c
          secondCardsList = (filter (\b@(nf,nc,nr) -> (length (nc!!fromJust(index))) < (length (c!!fromJust(index)))) newBoards)
        --  kToE = (filter (\b -> boardsNotEqual b board) (kingToEmpty board))
        --  rToC = (filter (\b -> boardsNotEqual b board) (resToColumns board))
        --  cToC = (filter (\b -> boardsNotEqual b board) (colToColumns board))
      --    cToRNoFilter = filter(\b -> (isJust (chooseMoveStupid b)) && (boardsNotEqual (fromJust (chooseMoveStupid b)) board)) (stackToReserves board)
        --  cToR = (filter (\b -> boardsNotEqual b board) (stackToReserves board))

  --Adds weights to the moves in the list by adding together the lengths of the columns and reserves
  addWeights::EOBoard->(EOBoard,Int)
  addWeights board@(f,c,r) = (board, (length r)^2 + (foldr (+) 0 [length col|col<-c]))

  boardsNotEqual::EOBoard->EOBoard->Bool
  boardsNotEqual board1@(f1,c1,r1) board2@(f2,c2,r2) = (f1/=f2) || (c1/=c2) || (r1/=r2)

  cardSecond::EOBoard -> Columns -> Maybe Int
  cardSecond _ [] = Nothing
  cardSecond board@(f,c,r) columns@(h:t)
    |(isJust(card h)) && isKing(fromJust (card h)) = (elemIndex h c)
    |(isJust(card h)) && not(isAce (fromJust (card h))) && ((filter (\found -> pCard (fromJust (card h)) == found) f) /= []) = (elemIndex h c)
    |otherwise = cardSecond board t
    where card col = if (length col >= 2) then (Just (col!!1)) else Nothing

  --Finds possible moves using all functions
  --Function that if king exposed, and theres an empty column, choose that move
  --Function that if move is made then toFoundations on new board is different,  use taht move LIST COMPS?
  findMoves :: EOBoard -> [EOBoard]
--  findMoves board = [toFoundations board|board<-newBoards, board/=([],[[]],[])]
  findMoves board@(f,c,r) = filter (boardsNotEqual toFBoard) (filter (\b -> b/=([],[[]],[])) newBoards)
    where newBoards = resToColumns toFBoard++colToColumns toFBoard ++kingToEmpty toFBoard ++stackToReserves toFBoard
          toFBoard = toFoundations (board)

  getLengths :: [EOBoard] -> [(EOBoard,Int)]
  getLengths boards = zip boards weights
    where weights = [cValues + length r|board@(f,c,r)<-boards]
          columns = [ c|board@(f,c,r)<-boards]
          cValues = foldr (+) 0 [length col|col<-columns]

  hello::EOBoard
  hello = ([],[[(Two,Spades),(Three,Spades)],[(Four,Spades)],[(Five,Spades)],[(Ace,Diamonds)],[],[],[],[]],[])

  bye::EOBoard
  bye = ([],[[(Two,Spades),(Three,Spades)],[(Four,Spades)],[(Five,Spades)],[(Ace,Diamonds)],[(King,Spades)],[],[],[]],[(Queen,Spades)])

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
    |board==newBoard = board
  --  |repeatedBoard board newBoard == True = ([],[[]],[])
    |otherwise = newBoard
    where newC = [(if (not(isKing card) && (not(null col)) && (sCard card == head col)) then card:col else col)|col<-c]
        --  newC = (map (\col -> if (not(isKing card) && sCard card == head col) then card:col else col) c)
          cHeads = [head n|n<-newC, not(null n)]
          newBoard = (f,newC,(filter (\res -> (not(elem res cHeads))) r))

  repeatedBoard::EOBoard->EOBoard->Bool
  repeatedBoard old new
  --  |((filter (\b -> b==old) (stackToReserves new)) /= []) = True
  --  |[if b==old then b else ([],[[]],[])|b<-findMoves new, b/=([],[[]],[])]
    |((filter (\b -> b==old) (findMoves new)) /= []) = True
    |otherwise = False
    where newBoards = stackToReserves new

------------------------------------------------------------------------------------------------------

  kingToEmpty::EOBoard->[EOBoard]
  kingToEmpty board@(f,c,r)
    |not (elem [] c) = [board] --check if there is an empty column  WHY
  --  |(length c == 8) = [([],[[]],[])]
    |otherwise = (filter (\b -> b/=board) (resToEmpty board ++ colToEmpty board))

  resToEmpty::EOBoard->[EOBoard]
  resToEmpty board@(f,c,r) = [(f,kingNewC c card,newR card)|card<-kingCards,r/=[]]
    where kingCards = filter isKing r
          newR card = filter (/= card) r

  kingNewC::Columns->Card->Columns
  kingNewC [] _ = []
  kingNewC columns@(hc:tc) king  -- = [king]:columns
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
    |length r >= 8 = [board]
    |otherwise = [colToReservesA board (head col)|col<-c, not(null col)]

  colToReservesA::EOBoard->Card->EOBoard
  colToReservesA board@(f,c,r) card
    |board==newBoard = board
    |null c = board
    |otherwise = newBoard
    where newBoard = (f,map (\col -> if (not(null col)&&(head col == card)) then (tail col) else col) c,(card:r))

-----------------------------------------------------------------------------------------------------

  stackToReserves::EOBoard->[EOBoard]
  stackToReserves board@(f,c,r)
    -- |(kingToEmpty board) && (kingToEmpty board) /= [] = [([],[[]],[])]
    |length r >= 8 = [board]
    |otherwise = [(f,makeNewColumns c stack r,newReserves stack r)|stack<-stacks, length (stack) > 1] ++ [(f,makeNewColumns c stack r,newReserves stack r)|stack<-stacks, length (stack) ==1]
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
  getNewColumn stack col@(hc:tc) board@(f,c,r)
    |(not(isKing (last stack)) && (hc == sCard (last stack))) = stack++col
--    |(length col==2) && (length stack==1) && (canBeMoved c stack) = tail col
    |(length col==1) && not(canBeMoved c stack) = col
    |(isInfixOf stack col) && (canBeMoved c stack) = col \\ stack
    |otherwise = col

  canBeMoved::Columns->Deck->Bool
  canBeMoved _ [] = False
  canBeMoved ([]:_) _ = False
  canBeMoved columns@((h:t):tc) stack
    |null columns = False
    |null tc = if (not(isKing (last stack)) && (h == sCard (last stack))) then True else False
    |not(isKing (last stack)) && (h == sCard (last stack)) = True
    |otherwise = canBeMoved tc stack
