

{-general
  change names from f and l etc
  where H has a map/filter, use list comprehension or whatever
  try to make master function toFoundations idk how
  loots of testing
  sort out system random and system.data.list or whatever
  check all null cases for all functions, especially toFoundations
  make random seed be random, or use Phils stuff
  Change types from a bunch of decks to foundations, reserves etc accordingly
  -}
module SolitaireOne where

--  import System.Random
--  import Data.List
--  import Data.List.Split

  data Suit = Hearts | Diamonds | Clubs | Spades
              deriving (Eq,Show)
  data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
             deriving (Eq, Show, Enum)
  type Card = (Pip, Suit)
  type Deck = [Card]
  type Foundations = Deck
  type Columns = [Deck]
  type Reserves = Deck
  type EOBoard = (Foundations, Columns, Reserves)

--------------------------------------------------------------------------------------------------------
  listhing::Deck
  listhing = take 6 shuffle
--Random crap delete later
  foundation::Deck --put this in line with eODeal so can actually test
  foundation = [(Ace,Hearts),(Ace,Diamonds),(Ace,Clubs),(Ace,Spades)]

  --Returns the successor card of the card passed in
  sCard::Card -> Card
  sCard card = (succ (fst card),(snd card))

  --Returns the predecessor card of the card passed in
  pCard::Card -> Card
  pCard card = (pred (fst card),(snd card))

  --Takes a card and returns true if it is an ace, and false otherwise
  isAce::Card -> Bool
  isAce card = fst card == Ace

  --Takes a card and returns true if it is a king, and false otherwise
  isKing::Card -> Bool
  isKing card = fst card == King

  --Creates a sorted pack of 52 cards
  pack::Deck
  pack = (createSuitDeck [] Hearts) ++ (createSuitDeck [] Diamonds) ++ (createSuitDeck [] Clubs) ++ (createSuitDeck [] Spades)

  --Creates all of the cards for one particular suit
  createSuitDeck::Deck -> Suit -> Deck
  createSuitDeck deck suit
    --If it is an empty deck so far, start with the king of the suit
    |null deck = createSuitDeck [(King, suit)] suit
    --If the head is an ace, the full deck has been created so it is returned
    |isAce h = deck
    --Recurses with the predecessor of the current card's pip
    |otherwise = createSuitDeck ((pCard h):deck) suit
    where (h:t) = deck

  --Returns a shuffled deck
  shuffle::Deck
  shuffle = map fst (sortBy (\(_,x) (_,y) -> compare x y) (zip pack getInts))

  --function to get list of ints using random thing
  --getInts::[Int]
  --getInts = take 52 (randoms (mkStdGen 42)::[Int])

  --Stolen from Phil
  getInts::[Int]
  getInts = [-1413815254,-1061608874,-1917740473,-1657021025,-1880775037,2035860998,-637623249,-505720016,284711073,-50861994,1400652750,-1482683619,484131249,-809310050,-210300391,1327025203,-1460604393,-1746252676,-1272570677,-793916433,2110898577,1943820126,1100594482,1932054375,-2037322212,161763804,841243188,1591843789,1274090478,1622254071,1419808067,-285385319,-713065031,-418511541,1377049314,-2036402777,429849394,168983222,-540749153,2062321714,-137535921,1549570322,-1421425515,-90023650,12243376,-1931152578,-672344479,-529526395,482373815,1959926423,580734309,-991618397]

  --Splits the shuffled deck into a playable board
  eODeal::EOBoard
  eODeal = ([], chunksOf 6 (drop 4 shuffle), (take 4 shuffle))

  --Atm, gets head of the first columns (three of clubs or whatever it is)
  getColumnHeads::EOBoard -> Card
  getColumnHeads board = head (head c)
    where (f,c,r) = board
  --toFoundations::EOBoard -> EOBoard
  --toFoundations startBoard = [2*n|n<-list,n==succ item] no clue what this is

  --Returns the columns
  getColumns::EOBoard -> Columns
  getColumns board = c
    where (f,c,r) = board

{-}  getColumnHeads::EOBoard->Deck
  getColumnHeads board = [h|(h:t) <- c]
    where (f,c,r) = board-}

-----------------------------------------------------------------------------------------------
  --Create foundations - used by checkList to check if the card should be put into Foundations
  --Returns the tail card if yes, otherwise returns the original card to be put in foundations
  createFound::Card -> Card -> Card
  createFound f t
    |f == pCard t = t
    |otherwise = f

    --checkList list@(h:t) f = checkList t [sCard n|n<-f, n == pCard h]
    --checkList (h:t) f = checkList t (map (\x -> sCard x) f)

    {-master::EOBOard -> EOBoard
    master first
      |first == current? = current
      |god knows-}

  --For each item in the list, puts the successor card in if needed using createFound, otherwise keeps original card
  checkList::Foundations -> Deck -> Foundations
  checkList f [] = f
  checkList f l@(h:t)
    |isAce h = checkList (h:f) t
    |otherwise = checkList (map (\x -> (createFound x h)) f) t
    --Change to list comprehension maybe so doesn't look like copying

  --Attempts to remove the card from reserves if already in f
  removeFromReserves::EOBoard -> EOBoard
  removeFromReserves board = (newF,c,(filter (\x -> not(elem x newF))) r)
    where (f,c,r) = board
          newF = checkList f r --change newF to something else

  --list comp equivalent to filter maybe
  --(x|x<-r, not(elem x newF))

  --Attempts to remove head from column if already in f
  removeFromColumns::EOBoard -> EOBoard
  removeFromColumns board = (newF,map (\x -> checkHeads x newF) c,r)
    where (f,c,r) = board
          newF = checkList f [head n|n<-c] --this is the column heads
  --for each head of each column, checks if in Foundations (using other function)
  --if it is, return tail else return full thing
  --Try and change map to filter or list comp

  --first deck is column
  --if the head is in f, returns just the tail, otherwise returns full column
  checkHeads::Deck -> Deck -> Deck
  checkHeads (h:t) f
    |elem h f = t
    |otherwise = (h:t)



  {-checkColumns::Columns -> Foundations -> Foundations
  checkColumns [] f = f
  checkColumns c@(h:t) f
    |isAce (head h) = checkColumns t ((head h):f)
    |otherwise = checkColumns t (map (\x -> createFound x (head h)) f)-}
--  BOTH WORK SOMEHOW
  checkColumns::Columns -> Foundations -> Foundations
  checkColumns c f = checkList f columnHeads
    where columnHeads = [head n|n<-c] --doesn't cause errors
    --[h|(h:t)<-c] - from H
