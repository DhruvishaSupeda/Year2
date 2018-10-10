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
  connasse=[("hello", 7),("merde",2)]

  salaud::Bag String
  salaud=[("hello", 7),("merde",1),("goodbye",2)]

  wtaf::Bag String
  wtaf=[("WHY",4)]

  listToBag::Eq a => [a] -> Bag a
  listToBag list1 = listToBagA list1 bcreate

  listToBagA::Eq a => [a] -> Bag a -> Bag a
  listToBagA list1 bag
    |null list1 = bag --if the string list is empty
    |otherwise = listToBagA t (bagInsert h bag) --inserts the item into the bag then recurses
    where (h:t) = list1

  bagEqual::Eq a => Bag a -> Bag a -> Bool
  bagEqual bag1 bag2
    |null bag1 && null bag2 = True --if both bags empty,technically equal
    |null bag1 || null bag2 = False --if only one is empty, false
    |(itemExists item bag2) && (number == getNumberOfItem item bag2) =
      bagEqual (itemDelete item bag1) (itemDelete item bag2)
      --if it exists in the second bag, then recurse with deleted from both bags
    |otherwise = False
    where ((item,number):rbag) = bag1

  itemPut::Eq a => a -> Int -> Bag a -> Bag a
  itemPut item number bag = ((item, number):itemDelete item bag)

  bagInsert::Eq a => a -> Bag a -> Bag a
  bagInsert item bag
    |itemExists item bag = itemPut item (numberIncrement item bag) bag
    |otherwise = itemPut item 1 bag

  bagSum::Eq a => Bag a -> Bag a -> Bag a
  bagSum bag1 bag2
    |null bag1 && null bag2 = bcreate
    |null bag2 = bag1
    |null bag1 = bag2
    |itemExists item bag2 =
      bagSum (itemDelete item bag1) (itemPut item (number+(getNumberOfItem item bag2)) bag2)
    |otherwise =
      bagSum (itemDelete item bag1) (itemPut item number bag2)
    where ((item,number):rbag) = bag1

    --go through each item in first list
    --if it exists, insert items in second and first into new bag andrecurse
    --if it doesn't, add and recurse

  bagIntersection::Eq a=> Bag a -> Bag a -> Bag a
  bagIntersection bag1 bag2 = bagIntersectionA bag1 bag2 bcreate

  bagIntersectionA::Eq a=> Bag a -> Bag a -> Bag a -> Bag a
  bagIntersectionA bag1 bag2 bagnew
    |null bag1 = bagnew
    |null bag1 || null bag2 = bcreate
    |itemExists item bag2 =
      bagIntersectionA (itemDelete item bag1){----} bag2 {----}(itemPut item (smallestValue number (getNumberOfItem item bag2)) bagnew)
    |otherwise = bagIntersectionA (itemDelete item bag1) bag2 bagnew
    where ((item,number):rbag) = bag1

  --if it exitst and no1<no2, keep and recurse, using t1
  --if it exists and no2<no1,replace with no2 and recurse, using t1
  --if it doesn't exist, delete and recurse--}

  itemDelete::Eq a => a -> Bag a -> Bag a
  itemDelete item bag
    |null bag = bcreate
    |item==headItem = rbag
    |otherwise = ((headItem,number):itemDelete item rbag)
    where ((headItem,number):rbag)=bag

  getNumberOfItem::Eq a => a -> Bag a -> Int
  getNumberOfItem item bag
    |null bag=error"item not present"
    |headItem==item = number
    |otherwise = getNumberOfItem item rbag
    where ((headItem,number):rbag) = bag

  itemExists::Eq a => a-> Bag a -> Bool
  itemExists item bag
    |null bag=False
    |headItem==item = True
    |otherwise = itemExists item rbag
    where ((headItem,number):rbag) = bag

  numberIncrement::Eq a => a -> Bag a -> Int
  numberIncrement item bag = (getNumberOfItem item bag) + 1

  smallestValue::Int -> Int -> Int
  smallestValue int1 int2
    |int1>int2 = int2
    |int2>int1 = int1
    |otherwise = int1
