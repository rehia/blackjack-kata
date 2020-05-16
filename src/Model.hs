module Model where

data Card
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
  deriving (Show, Eq, Enum)

softPoints :: Card -> Int
softPoints Ace = 11
softPoints King = 10
softPoints Queen = 10
softPoints Jack = 10
softPoints Ten = 10
softPoints Nine = 9
softPoints Eight = 8
softPoints Seven = 7
softPoints Six = 6
softPoints Five = 5
softPoints Four = 4
softPoints Three = 3
softPoints Two = 2

hardPoints :: Card -> Int
hardPoints Ace = 1
hardPoints value = softPoints value

data Deck = Deck [Card]
  deriving (Show, Eq)

data Hand = Hand [Card]
  deriving (Show, Eq)

data Score
  = Soft Int
  | Hard Int
  | BlackJack
  | Busted
  deriving (Show)

instance Eq Score where
  (Hard a) == (Hard b) = a == b
  (Soft a) == (Soft b) = a == b
  (Hard a) == (Soft b) = a == b
  (Soft a) == (Hard b) = a == b
  BlackJack == BlackJack = True
  Busted == Busted = True
  _ == _ = False

instance Ord Score where
  compare (Hard a) (Hard b) = compare a b
  compare (Soft a) (Soft b) = compare a b
  compare (Hard a) (Soft b) = compare a b
  compare (Soft a) (Hard b) = compare a b
  compare BlackJack BlackJack = EQ
  compare BlackJack (Hard _) = GT
  compare BlackJack (Soft _) = GT
  compare (Soft _) BlackJack = LT
  compare (Hard _) BlackJack = LT
  compare _ Busted = GT
  compare Busted _ = LT


data Player = Player Hand
  deriving (Show, Eq)
data Dealer = Dealer Hand
  deriving (Show, Eq)

data Game = Game Player Dealer Deck

data Winner
  = PlayerWins
  | DealerWins
  | NoOneWins
  deriving (Show, Eq)
