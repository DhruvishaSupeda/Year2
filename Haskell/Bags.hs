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

  --listToBag::Googlenit? Idk
  --listToBag::[String]
  --so basically, go to thehead and if it exists, add one to the number
  --if it doesn't, make a new tuple with value of 1

  --go through whole list,conitnously using bag insert until you're
  --driven to jump off the arts tower

  --bagEqual::Maybe use Eq?? Maybe ==??

  bPut::String->Int->Bag->Bag
  bPut p number bag = ((p, number):bdelete p bag)
  --find it using bget
  --if it exists, add one to the number
  --if not, make a new tuple
  bagInsert::String->Bag->Bag
  bagInsert p bag
     |existsbitch p bag  = bPut p (bitchesbeincrementing p bag) bag
     |otherwise = bPut p 1 bag

  --bagSum:: DEFFO lol who knows

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
