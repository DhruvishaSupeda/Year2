
module SolitaireOne where

  import System.Random
  import Data.List
  import Data.List.Split

  data Suit = Hearts | Diamonds | Clubs | Spades
              deriving (Eq,Show)
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
             deriving (Eq, Show, Enum)
  type Card = (Pip, Suit)
  type Deck = [Card]
  type Foundations = [Deck]
  type Columns = [Deck]
  type Reserves = Deck
  type EOBoard = (Foundations, Columns, Reserves) --god knows

--------------------------------------------------------------------------------------------------------
  --for returning predecessor, use succ and pred of the enum types
  --for the Pip so you just find the value, then add the suit back

  listhing::Deck
  listhing = take 6 shuffle

  foundation::Deck
  foundation = [(Ace,Hearts),(Ace,Diamonds),(Ace,Clubs),(Ace,Spades)]


  sCard::Card -> Card
  sCard card = (succ (fst card),(snd card))

  pCard::Card -> Card
  pCard card = (pred (fst card),(snd card))

  isAce::Card -> Bool
  isAce card = fst card == Ace

  isKing::Card -> Bool
  isKing card = fst card == King

  pack::Deck
  pack = (createSuitDeck [] Hearts) ++ (createSuitDeck [] Diamonds) ++ (createSuitDeck [] Clubs) ++ (createSuitDeck [] Spades)

  createSuitDeck::Deck -> Suit -> Deck
  createSuitDeck deck suit
    |null deck = createSuitDeck [(King, suit)] suit --if not started, put king in and start
    |isAce h = deck --if head is ace, stop
    |otherwise = createSuitDeck ((pCard h):deck) suit --recurse with predecessor
    where (h:t) = deck

  shuffle::Deck
  shuffle = map fst (sortBy (\(_,x) (_,y) -> compare x y) (zip pack getInts))

  --function to get list of ints
  getInts::[Int]
  getInts = take 52 (randoms (mkStdGen 42)::[Int])

  eODeal::EOBoard
  eODeal = ([], chunksOf 6 (drop 4 shuffle), (take 4 shuffle))

  getColumns::[Int] -> [Int]
  getColumns board = [2*n|n<-board,n>3]

  getColumnHeads::EOBoard -> Card
  getColumnHeads board = head (head c)
    where (f,c,r) = board
  --toFoundations::EOBoard -> EOBoard
  --toFoundations startBoard = [2*n|n<-list,n==succ item]
-----------------------------------------------------------------------------------------------
  createFound::Card -> Card -> Card
  createFound f t
    |f == pCard t = t
    |otherwise = f

    --checkList list@(h:t) f = checkList t [sCard n|n<-f, n == pCard h]
    --checkList (h:t) f = checkList t (map (\x -> sCard x) f)

  checkList::Deck->Deck->Deck
  checkList f [] = f
  checkList f l@(h:t)
    | isAce h = checkList (h:f) t
    | otherwise = checkList (map (\x -> (createFound x h)) f) t
