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
  salaud=[("hello", 7),("merde",1)]

  listToBag::[String]->Bag
  listToBag shitface = listToBagA shitface bcreate

  listToBagA::[String]->Bag->Bag
  listToBagA shitballs bag
    |null shitballs = bag
    |otherwise = listToBagA t (bagInsert h bag)
    where (h:t) = shitballs

  --go through whole list,continuously using bag insert until you're
  --driven to jump off the arts tower

  bagEqual::Bag->Bag->Bool
  bagEqual scrot1 gooch2
    |null scrot1 && null gooch2 = True
    |null scrot1 || null gooch2 = False
    |existsbitch (fst h1) gooch2 && snd h1 == bget (fst h1) gooch2 = bagEqual (bdelete (fst h1) scrot1) (bdelete (fst h1) gooch2)
    |otherwise = False
    where (h1:t1) = scrot1
          (h2:t2) = gooch2

    --go through list 1
    --for each item,check ifitexists in thesecond
    --if it does, check number of occurrences is the same
    --if it is, go tonext item otherwise give false

  bPut::String->Int->Bag->Bag
  bPut p number bag = ((p, number):bdelete p bag)

  --find it using bget
  --if it exists, add one to the number
  --if not, make a new tuple
  bagInsert::String->Bag->Bag
  bagInsert p bag
     |existsbitch p bag = bPut p (bitchesbeincrementing p bag) bag
     |otherwise = bPut p 1 bag

  bagSum::Bag->Bag->Bag
  bagSum campbellisadipshit asscake
    |

    --go through each item in first list
    --

  --bagIntersection:: he skipped it in the lecture sooo

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
