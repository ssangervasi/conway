module SerialSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Conway.GameOfLife
import Conway.Seeds
import Conway.Serial

puts = putStrLn

spec :: Spec
spec = do
  describe "golToString" $ do
    context "with an empty game" $ do
      let gol = emptyGoL

      it "is an empty string" $ do
        golToString gol `shouldBe` ""

    context "with a glider" $ do
      let gol = glider

      it "matches the glider string" $ do
        puts "Output:"
        puts $ golToString gol
        golToString gol `shouldBe` gliderString

  describe "stringToGoL" $ do
    context "with an empty string" $ do
      let golString = ""

      it "is an empty board" $ do
        stringToGoL golString `shouldBe` emptyGoL

    context "with a glider" $ do
      let golString = gliderString

      it "matches the glider board" $ do
        puts "Output:"
        puts $ golString
        print glider
        print $ stringToGoL golString
        stringToGoL golString `shouldBe` glider

  describe "chunksOf" $ do
    let testTitle size list expected = 
          (    (show size)
            ++ " | "
            ++ (show list)
            ++ " -> "
            ++ (show expected)
          )

    context "size 1 of []" $ do
      let size = 1
          list = [] :: [Int]
          expected = [] :: [[Int]]
        
      it (testTitle size list expected) $ do
        chunksOf size list `shouldBe` expected

    context "size 1 of [1..3]" $ do
      let size = 1
          list = [1, 2, 3]
          expected = [[1], [2], [3]]
        
      it (testTitle size list expected) $ do
        chunksOf size list `shouldBe` expected

    context "size 2 of [1..5]" $ do
      let size = 2
          list = [1, 2, 3, 4, 5]
          expected = [[1, 2], [3, 4], [5]]
        
      it (testTitle size list expected) $ do
        chunksOf size list `shouldBe` expected
