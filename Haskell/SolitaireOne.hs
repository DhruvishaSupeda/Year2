
module SolitaireOne where

  import System.Random
  import Data.List

  data Suit = Hearts | Diamonds | Clubs | Spades
              deriving (Eq,Show)
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
             deriving (Eq, Show, Enum)
  type Card = (Pip, Suit)
  type Deck = [Card]
  type Foundations = Deck
  type Columns = [Deck]
  type Reserves = Deck
  type EOBoard = (Foundations, [Columns], Reserves) --god knows

  new::[[Int]]
  new=sequence [[1,2,3], [7,27,37]]

{-- --Create empty bag
  bcreate::Bag a
  bcreate=[]

  --Calls the auxiliary function listToBagA with an empty bag
  listToBag::Eq a => [a] -> Bag a
  listToBag list1 = listToBagA list1 bcreate

  listToBagA::Eq a => [a] -> Bag a -> Bag a
  listToBagA list bag
   --once/if the list is empty, returns the bag
    |null list = bag
   --inserts the item into the bag then recurses
    |otherwise = listToBagA t (bagInsert h bag) --inserts the item into the bag then recurses
    where (h:t) = list

  bagEqual::Eq a => Bag a -> Bag a -> Bool
  bagEqual bag1 bag2 --}

--------------------------------------------------------------------------------------------------------
  --for returning predecessor, use succ and pred of the enum types
  --for the Pip so you just find the value, then add the suit back
  sCard::Card -> Card
  sCard start
    |isKing start = start --is this correct???
    |otherwise = (succ pip,suit)
    where (pip,suit) = start

  pCard::Card -> Card
  pCard start
    |isAce start = start --idk if this is right though, like is that the pred???
    |otherwise = (pred pip,suit)
    where (pip,suit) = start

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

  shuffle::Deck->Deck
  shuffle pack = map fst (sortBy (\(_,x) (_,y) -> compare x y) (zip pack getInts))

  --function to get list of ints
  getInts::[Int]
  getInts = take 52 (randoms (mkStdGen 42)::[Int])
