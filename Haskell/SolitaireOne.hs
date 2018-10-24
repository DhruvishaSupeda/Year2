
module SolitaireOne where
  type Card = (String, String)
  type Deck = [(String,String)]
  type Foundations = [(String,String)]
  type Columns = [(String,String)]
  type Reserves =[(String,String)]
  type Suit = String
  type Pip = String
  type EOBoard = [Foundations, Columns, Reserves] --god knows

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
