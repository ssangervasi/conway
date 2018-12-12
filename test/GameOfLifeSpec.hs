module GameOfLifeSpec (spec) where

import           Test.Hspec

import           Conway.GameOfLife
import           Conway.Seeds


spec :: Spec
spec = do
  describe "show" $ do
    context "with an empty game" $ do
      let gol = GoL [[]]

      it "is an empty string" $ do
        show gol `shouldBe` ""

    context "with a glider" $ do
      let gol = glider

      it "matches the glider string" $ do
        show gol `shouldBe` gliderString

  describe "read" $ do
    context "with an empty string" $ do
      let golString = ""

      it "is an empty board" $ do
        read golString `shouldBe` GoL [[]]

    context "with a glider" $ do
      let golString = gliderString

      it "matches the glider string" $ do
        read golString `shouldBe` glider
