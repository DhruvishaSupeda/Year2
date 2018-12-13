module SolitaireTwo where

  import System.Random
  import Data.List
  import Data.List.Split
  import Debug.Trace
  import SolitaireOne
  import Data.Maybe
----------------------------------------------------------------------------------------
  --Plays 100 games, gives back average no of games won and score
  eOExpt::Int->(Int,Float)
  eOExpt number= ((length (filter (\s -> s==52) scores)),((fromIntegral (foldr (+) 0 scores) / fromIntegral(100))))
    where random = take 100 (randoms (mkStdGen number)::[Int]) --80=5, 3=4
          scores = (map (\random -> eOGame (eODeal random)) random)

  --Plays a game and returns a score, which is how many cards have been moved to foundations
  eOGame::EOBoard->Int
  eOGame board@(f,c,r)
    --If no more possible moves, returns the score and ends the games
    |isNothing (chooseMove board) = (52- (length r) - (foldr (+) 0 (map length c)))
    --If there is a possible move, recurses and plays that move
    |otherwise = eOGame (fromJust (chooseMove board)) --score not correct

----------------------------------------------------------------------------------------

  --Chooses a move out of list of EOBoards from findMoves
  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove board@(f,c,r)
    |null newBoards = Nothing
    --If toFoundations can be called on the board again, return that new board
    |boardsNotEqual board (toFoundations board) = Just (toFoundations board)
    --If there is a card as the second item in any of the columns that can be moved, make and choose that move
    |(isJust (cardSecond board c)) && (secondCardsList /= []) = Just (head secondCardsList)
    --Hierarchical approach to choose what type of move should be made next
    |(not (null kToE)) = Just (head kToE)
    |(not (null rToC)) = Just (head rToC)
    |(not (null cToC)) = Just (head cToC)
    |(not (null sToR))= Just (head sToR)
    --If no other possible moves, return Nothing
    |otherwise = Nothing
    where newBoards = (findMoves board)
          index = cardSecond board c
          --For each move, finds the move that reduces the column length (i.e. takes head of column off) at the index given and returns those moves
          secondCardsList = if isJust index then (filter (\b@(nf,nc,nr) -> isJust index && ((length (nc!!fromJust(index))) < (length (c!!fromJust(index))))) newBoards) else []
          cHeads = [head n|n<-c, not(null n) && not(isKing (head n))]
          kToE = filter (boardsNotEqual board) (kingToEmpty board)
          rToC = filter (boardsNotEqual board) (resToColumns board)
          cToC = filter (boardsNotEqual board) (colToColumns board)
          sToR = filter (boardsNotEqual board) (stackToReserves board)

  --Returns a boolean of whether two boards are equal (false) or not (true)
  boardsNotEqual::EOBoard->EOBoard->Bool
  boardsNotEqual board1@(f1,c1,r1) board2@(f2,c2,r2) = (f1/=f2) || (c1/=c2) || (r1/=r2)

  --Works out the index of any column that has a card second to the head that can be moved in that board
  cardSecond::EOBoard -> Columns -> Maybe Int
  cardSecond _ [] = Nothing
  cardSecond board@(f,c,r) columns@(h:t)
    --If it is a king, it could be moved to an empty column, so return the index of that column
    |(isJust(card h)) && isKing(fromJust (card h)) = (elemIndex h c)
    --If the second card is an ace or is the successor of a card in foundations, return the index of that column
    |(isJust (card h)) && (((elem (fromJust (card h)) sFound)) || isAce(fromJust(card h)))  = (elemIndex h c) --ACES
    |otherwise = cardSecond board t
    where card col = if (length col >= 2) then (Just (col!!1)) else Nothing
          sFound = [sCard found|found<-f, not(isKing found)]

  --Finds all possible moves using all functions to get moves
  findMoves :: EOBoard -> [EOBoard]
  findMoves board@(f,c,r) = [toFoundations b|b<-newBoards, boardsNotEqual board b]
    where newBoards = (kingToEmpty board) ++ (resToColumns board)++(colToColumns board) ++(stackToReserves board)

  --Takes a column and returns the stack, which is a list of consecutive cards, or if there aren't any consecutive, just the head
  getStack::[Card]->[Card]->[Card]
  getStack [] stack = stack
  getStack col@(h:t) [] = getStack t [h]
  getStack col@(h:t) stack
    |(not(isKing (last stack)) && ((sCard (last stack)) == h)) = getStack t (stack++[h])
    |otherwise = stack

----------------------------------------------------------------------------------------------------
  --Returns all possible moves of moving a card in reserves to a column
  resToColumns::EOBoard->[EOBoard]
  resToColumns board@(f,c,r) = [resToColumnsA board res|res<-r, not(null res), boardsNotEqual (resToColumnsA board res) board]

  --Aux function for resToColumns
  resToColumnsA::EOBoard->Card->EOBoard
  resToColumnsA board@(f,c,r) card
    --If no changes to the board, returns original board, otherwise returns new board
    |board==newBoard = board
    |otherwise = newBoard
          --Works out new columns by checking is reserve card is the predecessor of the head of the column, and adding accordingly
    where newC = [(if (not(isKing card) && (not(null col)) && (sCard card == head col)) then card:col else col)|col<-c]
          cHeads = [head n|n<-newC, not(null n)]
          --The new board with the new columns and new reserves
          newBoard = (f,newC,(filter (\res -> (not(elem res cHeads))) r))

------------------------------------------------------------------------------------------------------

  --Returns all boards where a king is moved to an empty column
  kingToEmpty::EOBoard->[EOBoard]
  kingToEmpty board@(f,c,r)
    --If there is no empty column, returns the original board
    |not (elem [] c) = []
    --Returns all the possible boards, not including the ones same as the original board
    |otherwise = (filter (\b -> b/=board) (resToEmpty board ++ colToEmpty board))

  --Finds any kings in reserves that can be moved to columns, and returns those boards
  resToEmpty::EOBoard->[EOBoard]
  resToEmpty board@(f,c,r) = [(f,kingNewC c card,newR card)|card<-kingCards,r/=[]]
    where kingCards = filter isKing r
          newR card = filter (/= card) r

  --Gets new columns after moving a king
  kingNewC::Columns->Card->Columns
  kingNewC [] _ = []
  kingNewC columns@(hc:tc) king
    |null hc = (king:hc):tc
    |otherwise = hc:kingNewC tc king

  --Gets any boards where a king is moved from one column to another
  colToEmpty::EOBoard->[EOBoard]
  colToEmpty board@(f,c,r) = [(f,kingNewC (newC card) card,r)|card <- kingCards]
    where cHeads = [head n|n<-c, not(null n)]
          kingCards = filter (\card -> isKing card) cHeads
          newC card = map (\col -> if (not(null col)&&(head col == card)) then (tail col) else col) c

-------------------------------------------------------------------------------------------------------------------

  --Moves a stack to reserves if possible
  stackToReserves::EOBoard->[EOBoard]
  stackToReserves board@(f,c,r)
    --If the reserves are too big to move any cards, return original board
    |length r >= 8 = []
    --Returns a list of boards for moving each stack for each column to the reserves
    |otherwise = [(f,strMakeNewColumns c stack r,strNewReserves stack r)|stack<-stacks, length (stack) > 1, (not (isKing (last stack)))] ++
      [(f,strMakeNewColumns c stack r,strNewReserves stack r)|stack<-stacks, length (stack) ==1, not(isKing (head stack))]
    --List of stacks from each column
    where stacks = [getStack col []|col<-c, not(null col)]

  --Remakes all of the columns in the board, removing the stack
  strMakeNewColumns::Columns->[Card]->[Card]->Columns
  strMakeNewColumns columns stack r = [strNewCol stack col r|col<-columns]

  --Makes a new column given the original column, the stack (as much as it can fit) and the reserves (for tail recursion)
  strNewCol::[Card]->[Card]->[Card]->[Card]
  strNewCol _ [] _ = []
  strNewCol [] col _ = col
  strNewCol stack@(h:t) col r
    |(isInfixOf [h] col) && ((length r) <= 8) = strNewCol t (col\\([h])) (r++[h])
    |otherwise = col

  --Makes the new reserves with as much of the stack as it can put in
  strNewReserves::[Card]->[Card]->[Card]
  strNewReserves [] r = r
  strNewReserves stack@(h:t) reserves
    |length reserves < 8 = strNewReserves t (reserves++([h]))
    |otherwise = reserves

-----------------------------------------------------------------------------------------------------

  --Moves stacks between columns if possible
  colToColumns::EOBoard->[EOBoard]
  colToColumns board@(f,c,r) = [(f,(ctcNewColumns stack c board),r)|stack<-stacks, ((f,(ctcNewColumns stack c board),r)/=board)]
    where stacks = [getStack col []|col<-c]

  --Returns all of the new columns after moving the stack given
  ctcNewColumns::[Card]->Columns->EOBoard->Columns
  ctcNewColumns stack c board= [ctcGetNewColumn stack col board|col<-c]

  --Creates a new column and adds or deletes the stack if needed
  ctcGetNewColumn::[Card]->[Card]->EOBoard->[Card]
  ctcGetNewColumn _ [] _ = []
  ctcGetNewColumn [] col _ = col
  ctcGetNewColumn stack col@(hc:tc) board@(f,c,r)
    --If the stack can be moved to the column given, move it
    |(not(isKing (last stack)) && (hc == sCard (last stack))) = stack++col
    --If the stack can't be moved, return the original column
    |(length col==1) && not(canBeMoved c stack) = col
    --If the stack exists in the column, remove it
    |(isInfixOf stack col) && (canBeMoved cNotNull stack) = col \\ stack
    |otherwise = col
    where cNotNull = filter (/= []) c

  --Checks if the stack can be moved at all to another column
  canBeMoved::Columns->[Card]->Bool
  canBeMoved _ [] = False
  canBeMoved ([]:_) _ = False
  canBeMoved columns@((h:t):tc) stack
    |null columns = False
    --If the stack can be moved, return true else false
    |null tc = if (not(isKing (last stack)) && (h == sCard (last stack))) then True else False
    |not(isKing (last stack)) && (h == sCard (last stack)) = True
    |otherwise = canBeMoved tc stack
