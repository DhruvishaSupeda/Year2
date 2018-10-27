
module SolitaireOne where
  data Suit = Hearts | Diamonds | Clubs | Spades
              deriving (Eq,Show)
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
             deriving (Eq, Show, Enum)
  type Card = (Pip, Suit)
  type Deck = [Card]
  type Foundations = [Card]
  type Columns = [Card]
  type Reserves =[Card]
  --type EOBoard = [(Foundations), (Columns), (Reserves)] --god knows


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

  --for returning predecesspr, use succ and pred of the enum types
  --for the Pip so you just find the value, then add the suit back
  sCard::Card -> Card
  sCard start
    |pip == King = (Ace,suit)
    |otherwise = (succ pip,suit)
    where (pip,suit) = start
