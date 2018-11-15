

{-general
  change names from f and l etc
  where H has a map/filter, use list comprehension or whatever
  try to make master function toFoundations idk how
  loots of testing
  sort out system random and system.data.list or whatever
  check all null cases for all functions, especially toFoundations
  make random seed be random, or use Phils stuff
  Change types from a bunch of decks to foundations, reserves etc accordingly
  -}
module SolitaireOne where

  import System.Random
  import Data.List
  import Data.List.Split

  data Suit = Hearts | Diamonds | Clubs | Spades
              deriving (Eq,Show,Ord)
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
             deriving (Eq, Show, Enum, Ord)
  type Card = (Pip, Suit)
  type Deck = [Card]
  type Foundations = Deck
  type Columns = [Deck]
  type Reserves = Deck
  type EOBoard = (Foundations, Columns, Reserves)

--------------------------------------------------------------------------------------------------------
  listhing::Deck
  listhing = take 6 shuffle
--Random crap delete later
  foundation::Deck --put this in line with eODeal so can actually test
  foundation = [(Ace,Hearts),(Ace,Diamonds),(Ace,Clubs),(Ace,Spades)]

  --Returns the successor card of the card passed in
  sCard::Card -> Card
  sCard card
    |isKing card = (Ace,(snd card))
    |otherwise = (succ (fst card),(snd card))

  --Returns the predecessor card of the card passed in
  pCard::Card -> Card
  pCard card
    |isAce card =(King,(snd card))
    |otherwise = (pred (fst card),(snd card))

  --Takes a card and returns true if it is an ace, and false otherwise
  isAce::Card -> Bool
  isAce card = fst card == Ace

  --Takes a card and returns true if it is a king, and false otherwise
  isKing::Card -> Bool
  isKing card = fst card == King

  --Creates a sorted pack of 52 cards
  pack::Deck
  pack = (createSuitDeck [] Hearts) ++ (createSuitDeck [] Diamonds) ++ (createSuitDeck [] Clubs) ++ (createSuitDeck [] Spades)

  --Creates all of the cards for one particular suit
  createSuitDeck::Deck -> Suit -> Deck
  createSuitDeck deck suit
    --If it is an empty deck so far, start with the king of the suit
    |null deck = createSuitDeck [(King, suit)] suit
    --If the head is an ace, the full deck has been created so it is returned
    |isAce h = deck
    --Recurses with the predecessor of the current card's pip
    |otherwise = createSuitDeck ((pCard h):deck) suit
    where (h:t) = deck

  --Returns a shuffled deck
  shuffle::Deck
  shuffle = map fst (sortBy (\(_,x) (_,y) -> compare x y) (zip pack getInts))

  --function to get list of ints using random thing
  getInts::[Int]
  getInts = take 52 (randoms (mkStdGen 45)::[Int])

  --Splits the shuffled deck into a playable board
  eODeal::EOBoard
  eODeal = ([], chunksOf 6 (drop 4 shuffle), (take 4 shuffle))

  eODeal2::EOBoard
  eODeal2 = ([], chunksOf 6 (drop 4 pack), (take 4 pack))

  eODeal3::EOBoard
  eODeal3 = ([], chunksOf 6 pack, [])

  --Returns the columns
  getColumns::EOBoard -> Columns
  getColumns board = c
    where (f,c,r) = board

-----------------------------------------------------------------------------------------------
  --Create foundations - used by getFoundations to check if the card should be put into Foundations
  --Returns the tail card if yes, otherwise returns the original card to be put in foundations
  {-createFound::Card -> Card -> Card
  createFound f t
    |f == pCard t = t
    |otherwise = f -}

    --getFoundations list@(h:t) f = getFoundations t [sCard n|n<-f, n == pCard h]
    --getFoundations (h:t) f = getFoundations t (map (\x -> sCard x) f)

  toFoundations::EOBoard -> EOBoard
  toFoundations initialBoard
    |initialBoard == newBoard = initialBoard
    |otherwise = toFoundations newBoard
    where newBoard = removeFromColumns (removeFromReserves initialBoard)

  --For each item in the list, puts the successor card in if needed using createFound, otherwise keeps original card
  getFoundations::Foundations -> Deck -> Foundations
  getFoundations f [] = f
  getFoundations f (h:t)
    |isAce h = getFoundations (h:f) t
    -- |otherwise = getFoundations (map (\x -> (createFound x h)) f) t
    |otherwise = getFoundations (map (\x -> (if x == pCard h then h else x)) f) t --originally had createFound
    --Change to list comprehension maybe so doesn't look like copying

  --Attempts to remove the card from reserves if already in f
  removeFromReserves::EOBoard -> EOBoard
  removeFromReserves board = (newF, c, (removeFromReservesA newF r))
  --removeFromReserves board = (newF, c, [x,y|x<-newF, y<-r, compare y x == GT])
  -- [y|x<-newF, y<-r, compare y x == GT]
  --removeFromReserves board = (newF,c,(filter (\x -> not(elem x newF))) r)
  --00 if compare  == LT or EQ, then
  --removeFromReserves board = (newF,c,map (\x -> if not(elem x newF then )))
  -- map (\x -> [y|y<-r, compare y x == GT])
    where (f,c,r) = board
          newF = getFoundations f r --change newF to something else

  --list comp equivalent to filter maybe
  --(x|x<-r, not(elem x newF))

  removeFromReservesA::Deck->Deck->Deck
  removeFromReservesA newF [] = []
  removeFromReservesA [] r = r
  --  |null h = newF
  --  |null _ [] = []
  removeFromReservesA newF@(h:t) r = removeFromReservesA t (filter (\x -> compare x h == GT) r)

  getColumnHeads::EOBoard->Deck
  getColumnHeads (f,c,r) = [head n|n<-c, not(null n)]


  --Attempts to remove head from column if already in f
  removeFromColumns::EOBoard -> EOBoard
  removeFromColumns board = (newF,(map (\x -> checkHeads x newF) c), r)
    where (f,c,r) = board
          newF = getFoundations f columnHeads --this is the column heads
          columnHeads = [head n|n<-c, not(null n)]
          --[head n|n<-c, not(null n)]
          --columns = filter (\x -> not(null x)) c
  --for each head of each column, checks if in Foundations (using other function)
  --if it is, return tail else return full thing
  --Try and change map to filter or list comp

{-  heads::EOBoard->Deck
  heads board = [head n|n<-c, not(null n)]
    where (f,c,r) = board-}

  --first deck is column
  --if the head is in f, returns just the tail, otherwise returns full column
  checkHeads::Deck -> Deck -> Deck
  checkHeads [] _ = []
  checkHeads (h:t) f
    |elem h f = t
    |((length (h:t) == 1) && (not(elem h f))) = t -- init (h:t)
    |otherwise = (h:t)

  {-checkColumns::Columns -> Foundations -> Foundations
  checkColumns [] f = f
  checkColumns c@(h:t) f
    |isAce (head h) = checkColumns t ((head h):f)
    |otherwise = checkColumns t (map (\x -> createFound x (head h)) f)-}
--  BOTH WORK SOMEHOW
  checkColumns::Columns -> Foundations -> Foundations
  checkColumns c f = getFoundations f columnHeads
    where columnHeads = [head n|n<-c, not(null (head n))]
    --[h|(h:t)<-c] - from H
