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

  listToBag::Eq a =>[a]->Bag a
  listToBag shitface = listToBagA shitface bcreate

  listToBagA::Eq a =>[a]->Bag a->Bag a
  listToBagA shitballs bag
    |null shitballs = bag --if the string list is empty
    |otherwise = listToBagA t (bagInsert h bag) --inserts the iteminto the bag then recurses
    where (h:t) = shitballs

  bagEqual::Bag a->Bag a->Bool
  bagEqual scrot1 gooch2
    |null scrot1 && null gooch2 = True --if both bags empty,technically equal
    |null scrot1 || null gooch2 = False --if only one is empty, false
    |itemExists (fst h1) gooch2 && snd h1 == bget (fst h1) gooch2 =
      bagEqual (bdelete (fst h1) scrot1) (bdelete (fst h1) gooch2)
      --if it exists in the second bag, then recurse with deleted from both bags
    |otherwise = False
    where (h1:t1) = scrot1
          (h2:t2) = gooch2

  bPut::Eq a =>a->Int->Bag a->Bag a
  bPut p number bag = ((p, number):bdelete p bag)

  bagInsert::Eq a =>a->Bag a->Bag a
  bagInsert p bag
    |itemExists p bag = bPut p (numberIncrement p bag) bag
    |otherwise = bPut p 1 bag

  bagSum::Bag a->Bag a->Bag a
  bagSum bag1 bag2
    |null bag1 && null bag2 = bcreate
    |null bag1 = bag2
    |itemExists (fst h1) bag2 =
      bagSum (bdelete (fst h1) bag1) (bPut (fst h1) (snd h1+(bget (fst h1) bag2)) bag2)
    |otherwise =
      bagSum (bdelete (fst h1) bag1) (bPut (fst h1) (snd h1) bag2)
    where (h1:t1) = bag1

    --go through each item in first list
    --if it exists, insert items in second and first into new bag andrecurse
    --if it doesn't, add and recurse

  bagIntersection::Bag a->Bag a->Bag a
  bagIntersection bag1 bag2
    |null t1 = bag1
    |null bag1 || null bag2 = bcreate --if one or more is empty, no intersection
    |itemExists (fst h1) bag2 && (snd h1)<(bget (fst h1) bag2) =
      (h1:bagIntersection t1 bag2)
    |itemExists (fst h1) bag2 && ((bget (fst h1) bag2)<(snd h1)) =
      ((bPut (fst h1) (bget (fst h1) bag2) bag1):(bagIntersection t1 bag2))
    |otherwise = bagIntersection t1 bag2
    where (h1:t1) = bag1

    --WHY

--if it exitst and no1<no2, keep and recurse, using t1
--if it exists and no2<no1,replace with no2 and recurse, using t1
--if it doesn't exist, delete and recurse--}

  {--bagIntersection::Bag->Bag->Bag
  bagIntersection (h1:t1) bag2
    |null t1 = (h1:t1)
    |null (h1:t1) || null bag2 = bcreate
    |itemExists (fst h1) bag2 = (h1:bagIntersection t1 bag2)
    |otherwise = bagIntersection t1 bag2--}

  bdelete::Eq a => a->Bag a->Bag a
  bdelete p bag
    |null bag = []
    |p==q = rbag
    |otherwise = ((q,number):bdelete p rbag)
    where ((q,number):rbag)=bag

  bget::Eq a=> a->Bag a->Int
  bget q bag
    |null bag=error"item not present"
    |p==q = number
    |otherwise = bget q rbag
    where ((p,number):rbag) = bag

  itemExists::Eq a=>a->Bag a->Bool
  itemExists item bag
    |null bag=False
    |p==item = True
    |otherwise = itemExists item rbag
    where ((p,number):rbag) = bag

  numberIncrement::Eq a=>a->Bag a->Int
  numberIncrement fuckoff bag = (bget fuckoff bag) + 1
