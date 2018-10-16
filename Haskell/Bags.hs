{-**********************
Bags.hs
Assignment hell hi Phil
**********************-}
module Bags where
  type Bag a = [(a, Int)]

 --Create empty bag
  bcreate::Bag a
  bcreate=[]

  connasse::Bag String
  connasse=[("hello", 7),("merde",4),("goodbyeeeeeeeeeeee",8)]

  salaud::Bag String
  salaud=[("hellooooooooooo", 7),("merde",1),("goodbye",2)]

 --Bags used for testing
  testBag1::Bag String
  testBag1 = [("test",3),("original",1),("generic string",15)]

  testBag2::Bag Int
  testBag2 = [(7,3),(34,9),(7,1)]

  testBag3::Bag String
  testBag3 = [("test",3),("original",1),("generic string",15)]

  testBag4::Bag String
  testBag4 = [("original",1),("test",3),("generic string",15)]

  testBag5::Bag String
  testBag5 = [("hello",7),("test",4),("generic string",15)]

  emptyBag::Bag String
  emptyBag = []

  --Calls the auxiliary function listToBagA with an empty bag
  listToBag::Eq a => [a] -> Bag a
  listToBag list1 = listToBagA list1 bcreate

  listToBagA::Eq a => [a] -> Bag a -> Bag a
  listToBagA list1 bag
   --once/if the list is empty, returns the bag
    |null list1 = bag
   --inserts the item into the bag then recurses
    |otherwise = listToBagA t (bagInsert h bag) --inserts the item into the bag then recurses
    where (h:t) = list1

  bagEqual::Eq a => Bag a -> Bag a -> Bool
  bagEqual bag1 bag2
   --if both bags empty, technically equal so returns true
    |null bag1 && null bag2 = True
   --if only one is empty, return false as they are not equal
    |null bag1 || null bag2 = False
   --if the item in the first bag exists in the second bag, delete from both bags and recurse
    |(itemExists item bag2) && (number == getNumberOfItem item bag2) =
      bagEqual (itemDelete item bag1) (itemDelete item bag2)
    |otherwise = False
    where ((item,number):rbag) = bag1
          ((item2,number2):t2) = bag2

  --inserts an item into a bag, given the item and number of occurences
  itemPut::Eq a => a -> Int -> Bag a -> Bag a
  itemPut item number bag = ((item, number):itemDelete item bag)

  bagInsert::Eq a => a -> Bag a -> Bag a
  bagInsert item bag
   --if the item already exists, add one to the number of occurences
    |itemExists item bag = itemPut item (numberIncrement item bag) bag
   --if the item doesn't exist, make a new tuple with an occurence of 1
    |otherwise = itemPut item 1 bag

  bagSum::Eq a => Bag a -> Bag a -> Bag a
  bagSum bag1 bag2
   --returns empty bag if both bags are null
    |null bag1 && null bag2 = bcreate
   --if one bag is null, returns the other bag
    |null bag2 = bag1
    |null bag1 = bag2
   {--if the item exists in both bags, adds up the total no. of occurences into the second bag
   and deletes from the first and recurses--}
    |itemExists item bag2 =
      bagSum (itemDelete item bag1) (itemPut item (number+(getNumberOfItem item bag2)) bag2)
   --if the item in the first bag doesn't exist in the second, deletes in the first and recurses
    |otherwise =
      bagSum (itemDelete item bag1) (itemPut item number bag2)
    where ((item,number):rbag) = bag1


--  bagIntersection::Eq a=> Bag a -> Bag a -> Bag a
--  bagIntersection bag1 bag2 = bagIntersectionA bag1 bag2 bcreate

  bagIntersectionA::Eq a=> Bag a -> Bag a -> Bag a -> Bag a
  bagIntersectionA bag1 bag2 bagnew
    |null bag1 = bagnew
    |null bag1 || null bag2 = bcreate
    |itemExists item bag2 =
      bagIntersectionA (itemDelete item bag1){----} bag2 {----}(itemPut item (smallestValue number (getNumberOfItem item bag2)) bagnew)
    |otherwise = bagIntersectionA (itemDelete item bag1) bag2 bagnew
    where ((item,number):rbag) = bag1

  bagIntersection::Eq a=> Bag a -> Bag a -> Bag a
  bagIntersection [] _ = []
  bagIntersection _ [] = []
  bagIntersection bag1 bag2
  --  |itemExists (fst h1) bag2 = (h1:bagIntersection t1 bag2)
  --  |otherwise = bagIntersection t1 bag2
  {--  |itemExists item bag2 && number < getNumberOfItem item bag2 =
      (h1:bagIntersection t1 bag2)
    |itemExists item bag2 && number > getNumberOfItem item bag2 =
      (itemPut item (getNumberOfItem item bag2) bag1) ++ (bagIntersection t1 bag2) --}
    |itemExists item bag2 =
      (itemPut item (smallestValue number (getNumberOfItem item bag2)) bag1) ++ (bagIntersection (itemDelete item bag1) bag2)
    |otherwise = bagIntersection t1 bag2
    where ((item,number):t1) = bag1

  --if it exitst and no1<no2, keep and recurse, using t1
  --if it exists and no2<no1,replace with no2 and recurse, using t1
  --if it doesn't exist, delete and recurse

  --function used to delete an item
  itemDelete::Eq a => a -> Bag a -> Bag a
  itemDelete item bag
    |null bag = bcreate
    |item==headItem = rbag
    |otherwise = ((headItem,number):itemDelete item rbag)
    where ((headItem,number):rbag)=bag

  --function used to get the number of occurences in a bag given the item
  getNumberOfItem::Eq a => a -> Bag a -> Int
  getNumberOfItem item bag
    |null bag=error"item not present"
    |headItem==item = number
    |otherwise = getNumberOfItem item rbag
    where ((headItem,number):rbag) = bag

  --checks whether an item exists in a bag, and returns true if it does
  itemExists::Eq a => a-> Bag a -> Bool
  itemExists item bag
    |null bag=False
    |headItem==item = True
    |otherwise = itemExists item rbag
    where ((headItem,number):rbag) = bag

  --increments the number of occurences of an item in a bag
  numberIncrement::Eq a => a -> Bag a -> Int
  numberIncrement item bag = (getNumberOfItem item bag) + 1

  --given two integers, returns the smallest
  smallestValue::Int -> Int -> Int
  smallestValue int1 int2
    |int1>int2 = int2
    |int2>int1 = int1
    |otherwise = int1
