module Model where

data CardValue
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
  deriving (Show, Eq)

cardPoints :: CardValue -> Int
cardPoints Ace = 11
cardPoints King = 10
cardPoints Queen = 10
cardPoints Jack = 10
cardPoints Ten = 10
cardPoints Nine = 9
cardPoints Eight = 8
cardPoints Seven = 7
cardPoints Six = 6
cardPoints Five = 5
cardPoints Four = 4
cardPoints Three = 3
cardPoints Two = 2

hardCardPoints :: CardValue -> Int
hardCardPoints Ace = 1
hardCardPoints value = cardPoints value

cardValues :: [CardValue]
cardValues =
  [ Ace
  , King
  , Queen
  , Jack
  , Ten
  , Nine
  , Eight
  , Seven
  , Six
  , Five
  , Four
  , Three
  , Two
  ]

data Color
  = Hearts
  | Clubs
  | Diamonds
  | Spades
  deriving (Show, Eq)

colors :: [Color]
colors = [ Hearts, Clubs, Diamonds, Spades ]

data Card = Card CardValue Color
  deriving (Show, Eq)

softPoints :: Card -> Int
softPoints (Card value _) = cardPoints $ value

hardPoints :: Card -> Int
hardPoints (Card value _) = hardCardPoints $ value

data Deck = Deck [Card]

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
  compare BlackJack (Hard _) = GT
  compare BlackJack (Soft _) = GT

data Player = Player Hand
  deriving (Show, Eq)
data Dealer = Dealer Hand
  deriving (Show, Eq)

data Game = Game Deck Player Dealer

data Winner
  = PlayerWins
  | DealerWins
  | NoOneWins
  deriving (Show, Eq)
