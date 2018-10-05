{-**********************
Bags.hs
Assignment hell hi Phil
**********************-}
module Bags where
  type Bag = [(String, Int)]

 --Create empty bag
  bcreate::Bag
  bcreate=[]

  --listToBag::Googlenit? Idk
--  listToBag

  --bagEqual::Maybe use Eq?? Maybe ==??

  bagInsert::String->Int->Bag->Bag
  bagInsert p number bag = ((p, number):bdelete p bag)

  --bagSum:: DEFFO lol who knows

  --bagIntersection:: he skipped it in the lecture sooo

  bdelete::String->Bag->Bag
  bdelete p bag
    |null bag = []
    |p==q = rbag
    |otherwise = ((q,number):bdelete p rbag)
    where ((q,number):rbag)=bag

  bget::String->Bag->String
  bget q bag
    |null bag=error"item not present"
    |p==q = q
    |otherwise = bget q rbag
    where ((p,number):rbag) = bag
