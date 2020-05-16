module Spec where

import           Blackjack
import           Model
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "init game" $ do
    it "should initialize a game with a full deck" $ do
      let Game (Deck deck) _ _ = initGame
      length deck `shouldBe` 52
    it "should initialize a player with an empty hand" $ do
      let Game _ (Player (Hand hand)) _ = initGame
      length hand `shouldBe` 0
    it "should initialize a dealer with an empty hand" $ do
      let Game _ _ (Dealer (Hand hand)) = initGame
      length hand `shouldBe` 0
  describe "deal a card" $ do
    it "should deal a card to player" $ do
      let Game (Deck deck) (Player (Hand hand)) _ = dealCardToPlayer initGame
      length deck `shouldBe` 51
      length hand `shouldBe` 1
    it "should deal a card to dealer" $ do
      let Game (Deck deck) _ (Dealer (Hand hand)) = dealCardToDealer initGame
      length deck `shouldBe` 51
      length hand `shouldBe` 1
  describe "calculate hand score" $ do
    it "should be hard 0 when no card" $ do
      let hand = Hand mempty
      score hand `shouldBe` Hard 0
    it "should be hard 10 when only one King" $ do
      let hand = Hand [Card King Hearts]
      score hand `shouldBe` Hard 10
    it "should be hard 20 when one Queen and a Jack" $ do
      let hand = Hand [Card Queen Hearts, Card Jack Hearts]
      score hand `shouldBe` Hard 20
    it "should be a blackjack when 21" $ do
      let hand = Hand [Card Queen Hearts, Card Nine Hearts, Card Two Hearts]
      score hand `shouldBe` BlackJack
    it "should be a busted when over 21" $ do
      let hand = Hand [Card Queen Hearts, Card Jack Hearts, Card Two Hearts]
      score hand `shouldBe` Busted
    it "should be soft 11 when one ace" $ do
      let hand = Hand [Card Ace Hearts]
      score hand `shouldBe` Soft 11
    it "should be soft 20 when one ace and 9" $ do
      let hand = Hand [Card Ace Hearts, Card Nine Hearts]
      score hand `shouldBe` Soft 20
    it "should be hard 20 when one ace with king and nine" $ do
      let hand = Hand [Card Ace Hearts, Card King Hearts, Card Nine Hearts]
      score hand `shouldBe` Hard 20
  describe "determine winner" $ do
    it "player should win if has a better score than dealer" $ do
      let player = Player . Hand $ [Card King Hearts, Card Queen Hearts]
      let dealer = Dealer . Hand $ [Card King Hearts, Card Seven Hearts]
      let game = Game (Deck mempty) player dealer
      winner game `shouldBe` PlayerWins
    it "player should loose if busted" $ do
      let player = Player . Hand $ [Card King Hearts, Card Queen Hearts, Card Two Hearts]
      let dealer = Dealer . Hand $ [Card King Hearts, Card Seven Hearts]
      let game = Game (Deck mempty) player dealer
      winner game `shouldBe` DealerWins
    it "player should loose if dealer has a better score" $ do
      let player = Player . Hand $ [Card King Hearts]
      let dealer = Dealer . Hand $ [Card King Hearts, Card Two Hearts]
      let game = Game (Deck mempty) player dealer
      winner game `shouldBe` DealerWins
    it "no one should win if score are the same" $ do
      let player = Player . Hand $ [Card King Hearts, Card Nine Hearts]
      let dealer = Dealer . Hand $ [Card Jack Hearts, Card Seven Hearts, Card Two Hearts]
      let game = Game (Deck mempty) player dealer
      winner game `shouldBe` NoOneWins
    it "player should loose if busted, even if dealer is busted" $ do
      let player = Player . Hand $ [Card King Hearts, Card Queen Hearts, Card Two Hearts]
      let dealer = Dealer . Hand $ [Card King Hearts, Card Queen Hearts, Card Two Hearts]
      let game = Game (Deck mempty) player dealer
      winner game `shouldBe` DealerWins
