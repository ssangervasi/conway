module GameOfLifeSpec (spec) where

import Test.Hspec

import Conway.GameOfLife
import Conway.Seeds


spec :: Spec
spec = do
  describe "nextCellState " $ do
    context "of a live cell" $ do
      context "with zero live neighbors" $ do
        it "is dead" $ do pending
      -- context "with zero live neighbors"
      -- context "with one live neighbor"
      -- context "with three live neighbors"
      -- context "with three live neighbors"
      -- context "with four live neighbors"

    context "of a dead cell" $ do
      it "is dead" $ pending
    
    