

{-general
  change names from f and l etc
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

  --Function to get list of ints using random generator
  getInts::[Int]
  getInts = take 52 (randoms (mkStdGen 42)::[Int])

  --Splits the shuffled deck into a playable board
  eODeal::EOBoard
  eODeal = ([], chunksOf 6 (drop 4 shuffle), (take 4 shuffle))

  eODeal2::EOBoard
  eODeal2 = ([], chunksOf 6 (drop 4 pack), (take 4 pack))

  eODeal3::EOBoard
  eODeal3 = ([], chunksOf 6 pack, [])

-----------------------------------------------------------------------------------------------
  toFoundations::EOBoard -> EOBoard
  toFoundations initialBoard
    |initialBoard == newBoard = initialBoard
    |otherwise = toFoundations newBoard
    where newBoard = removeFromColumns (removeFromReserves initialBoard)

  --For each item in the list, replaces with successor card if needed, otherwise keeps original card
  getFoundations::Foundations -> Deck -> Foundations
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
  removeFromReservesA::Deck->Deck->Deck
  removeFromReservesA newF [] = []
  removeFromReservesA [] r = r
  removeFromReservesA (h:t) r = removeFromReservesA t (filter (\x -> compare x h == GT) r)

  --Removes head from column if already in foundations, else returns the entire column
  removeFromColumns::EOBoard -> EOBoard
  removeFromColumns board = (newF,(map (\x -> checkHeads x newF) c), r)
    where (f,c,r) = board
          newF = getFoundations f columnHeads --this is the column heads
          columnHeads = [head n|n<-c, not(null n)]

  --first deck is column
  --if the head is in f, returns just the tail, otherwise returns full column
  checkHeads::Deck -> Deck -> Deck
  checkHeads [] _ = []
  checkHeads (h:t) f
    --If the card is in foundations, remove by returning the list without it
    |elem h f = t
    --If only the head remains, and the head is not in foundations, return the head
    |((length (h:t) == 1) && (not(elem h f))) = init (h:t)
    --Otherwise, return entire list
    |otherwise = (h:t)
