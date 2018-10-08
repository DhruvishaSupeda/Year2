{-**********************
Bags.hs
Assignment hell hi Phil
**********************-}
module Bags where
  type Bag = [(String, Int)]

 --Create empty bag
  bcreate::Bag
  bcreate=[]

  connasse::Bag
  connasse=[("hello", 7),("merde",1)]

  salaud::Bag
  salaud=[("hello", 5),("merde",1),("goodbye",2)]

  listToBag::[String]->Bag
  listToBag shitface = listToBagA shitface bcreate

  listToBagA::[String]->Bag->Bag
  listToBagA shitballs bag
    |null shitballs = bag --if the string list is empty
    |otherwise = listToBagA t (bagInsert h bag) --inserts the iteminto the bag then recurses
    where (h:t) = shitballs

  bagEqual::Bag->Bag->Bool
  bagEqual scrot1 gooch2
    |null scrot1 && null gooch2 = True --if both bags empty,technically equal
    |null scrot1 || null gooch2 = False --if only one is empty, false
    |existsbitch (fst h1) gooch2 && snd h1 == bget (fst h1) gooch2 =
      bagEqual (bdelete (fst h1) scrot1) (bdelete (fst h1) gooch2)
      --if it exists in the second bag, then recurse with deleted from both bags
    |otherwise = False
    where (h1:t1) = scrot1
          (h2:t2) = gooch2

  bPut::String->Int->Bag->Bag
  bPut p number bag = ((p, number):bdelete p bag)

  bagInsert::String->Bag->Bag
  bagInsert p bag
    |existsbitch p bag = bPut p (bitchesbeincrementing p bag) bag
    |otherwise = bPut p 1 bag

  bagSum::Bag->Bag->Bag
  bagSum campbellisadipshit asscake
    |null campbellisadipshit && null asscake = bcreate
    |null campbellisadipshit = asscake
    |existsbitch (fst h1) asscake =
      bagSum (bdelete (fst h1) campbellisadipshit) (bPut (fst h1) (snd h1+(bget (fst h1) asscake)) asscake)
    |otherwise =
      bagSum (bdelete (fst h1) campbellisadipshit) (bPut (fst h1) (snd h1) asscake)
    where (h1:t1) = campbellisadipshit
          (h2:t2) = asscake


    --go through each item in first list
    --if it exists, insert items in second and first into new bag andrecurse
    --if it doesn't, add and recurse

  bagIntersection::Bag->Bag->Bag
  bagIntersection bag1 bag2
  

  bdelete::String->Bag->Bag
  bdelete p bag
    |null bag = []
    |p==q = rbag
    |otherwise = ((q,number):bdelete p rbag)
    where ((q,number):rbag)=bag

  bget::String->Bag->Int
  bget q bag
    |null bag=error"item not present"
    |p==q = number
    |otherwise = bget q rbag
    where ((p,number):rbag) = bag

  existsbitch::String->Bag->Bool
  existsbitch stringgggggggg bag
    |null bag=False
    |p==stringgggggggg = True
    |otherwise = existsbitch stringgggggggg rbag
    where ((p,number):rbag) = bag

  bitchesbeincrementing::String->Bag->Int
  bitchesbeincrementing fuckoff bag = (bget fuckoff bag) + 1
