module Blackjack where

import           Model

initGame :: Game
initGame = Game newDeck (Player . Hand $ mempty) (Dealer . Hand $ mempty)

newDeck :: Deck
newDeck = Deck $ replicate 4 =<< [Ace .. Two]

dealCardToPlayer :: Game -> Game
dealCardToPlayer (Game (Deck (card:deck)) (Player (Hand hand)) dealer) =
  Game (Deck deck) (Player . Hand $ (card:hand)) dealer

dealCardToDealer :: Game -> Game
dealCardToDealer (Game (Deck (card:deck)) player (Dealer (Hand hand))) =
  Game (Deck deck) player (Dealer . Hand $ (card:hand))

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
winner (Game _ (Player playerHand) (Dealer dealerHand))
  | playerScore == Busted = DealerWins
  | dealerScore == playerScore = NoOneWins
  | dealerScore > playerScore = DealerWins
  | otherwise = PlayerWins
  where
    playerScore = score playerHand
    dealerScore = score dealerHand
