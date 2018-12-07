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

  --Returns the successor card of the card passed in
  sCard::Card -> Card
  sCard card = (succ (fst card),(snd card))

  --Returns the predecessor card of the card passed in
  pCard::Card -> Card
  pCard card = (pred (fst card),(snd card))

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
  createSuitDeck::[Card] -> Suit -> Deck
  createSuitDeck deck suit
    --If it is an empty deck, start with the king of the suit
    |null deck = createSuitDeck [(King, suit)] suit
    --If the head is an ace, the full deck has been created so it is returned
    |isAce h = deck
    --Recurses with the predecessor of the current card's pip
    |otherwise = createSuitDeck ((pCard h):deck) suit
    where (h:t) = deck

  --Returns a shuffled deck
  {-shuffle::Int->Deck
  shuffle seed = map fst (sortBy (\(_,x) (_,y) -> compare x y) (zip pack getInts seed))

  --Function to get list of random ints
  getInts::Int->[Int]
  getInts seed = take 52 (randoms (mkStdGen seed)::[Int])

  --Splits the shuffled deck into a playable board
  eODeal::Int->EOBoard
  eODeal seed = ([], chunksOf 6 (drop 4 shuffle seed), (take 4 shuffle seed))-}

  --Main toFoundations function
  toFoundations::EOBoard -> EOBoard
  toFoundations initialBoard
    --If no new moves have been made, return the board
    |initialBoard == newBoard = initialBoard
    --Recurse with new board
    |otherwise = toFoundations newBoard
    --newBoard is a board where all possible moves from columns and reserves have been made on the passed in board
    where newBoard = removeFromColumns (removeFromReserves initialBoard)

  --For each item in the list, replaces with successor card if needed, otherwise keeps original card
  getFoundations::Foundations -> [Card] -> Foundations
  getFoundations f [] = f
  getFoundations f (h:t)
    |isAce h = getFoundations (h:f) t
    |otherwise = getFoundations (map (\x -> (if x == pCard h then h else x)) f) t

  --Removes the card from reserves if already in foundations
  removeFromReserves::EOBoard -> EOBoard
  removeFromReserves board = (newF, c, (removeFromReservesA newF r))
    where (f,c,r) = board
          newF = getFoundations f r

  --Recurses through the foundations, removing from reserves if the card (and predecessors) exist in foundations
  removeFromReservesA::[Card]->[Card]->[Card]
  removeFromReservesA newF [] = []
  removeFromReservesA [] r = r
  removeFromReservesA (h:t) r = removeFromReservesA t (filter (\x -> compare x h == GT) r)

  --Removes head from column if already in foundations, else returns the entire column
  removeFromColumns::EOBoard -> EOBoard
  removeFromColumns board = (newF,(map (\x -> checkHeads x newF) c), r)
    where (f,c,r) = board
          newF = getFoundations f columnHeads --this is the column heads
          columnHeads = [head n|n<-c, not(null n)]

  --If the head is in f, returns just the tail, otherwise returns full column
  checkHeads::[Card] -> [Card] -> [Card]
  checkHeads [] _ = []
  checkHeads (h:t) f
    --If the card is in foundations, remove by returning the list without it
    |elem h f = t
    --If only the head remains, and the head is not in foundations, return the head
    |((length (h:t) == 1) && (not(elem h f))) = init (h:t)
    --Otherwise, return entire list
    |otherwise = (h:t)
