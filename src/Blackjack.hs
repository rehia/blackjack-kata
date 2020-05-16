module Blackjack where

import           Model
import Control.Monad.Random.Class (MonadRandom)
import System.Random.Shuffle (shuffleM)

initGame :: MonadRandom m => m Game
initGame = Game newPlayer newDealer <$> shuffleDeck newDeck
  where
    newPlayer = Player . Hand $ mempty
    newDealer = Dealer . Hand $ mempty
    newDeck = Deck $ replicate 4 =<< [Ace .. Two]

shuffleDeck :: MonadRandom m => Deck -> m Deck
shuffleDeck (Deck deck) = Deck <$> shuffleM deck

dealCardToPlayer :: Game -> Game
dealCardToPlayer (Game (Player (Hand hand)) dealer (Deck (card:deck))) =
  Game (Player . Hand $ (card:hand)) dealer (Deck deck)

dealCardToDealer :: Game -> Game
dealCardToDealer (Game player (Dealer (Hand hand)) (Deck (card:deck))) =
  Game player (Dealer . Hand $ (card:hand)) (Deck deck)

score :: Hand -> Score
score (Hand hand)
  | softResult == 21 = BlackJack
  | Ace `elem` hand && softResult < 21 = Soft softResult
  | hardResult < 21 = Hard hardResult
  | otherwise   = Busted
  where
    softResult = sum $ softPoints <$> hand
    hardResult = sum $ hardPoints <$> hand

winner :: Game -> Winner
winner (Game (Player playerHand) (Dealer dealerHand) _)
  | playerScore == Busted = DealerWins
  | dealerScore == playerScore = NoOneWins
  | dealerScore > playerScore = DealerWins
  | otherwise = PlayerWins
  where
    playerScore = score playerHand
    dealerScore = score dealerHand
