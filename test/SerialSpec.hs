module SerialSpec (spec) where

import Test.Hspec

import Conway.GameOfLife
import Conway.Seeds
import Conway.Serial

spec :: Spec
spec = do
  describe "golToString" $ do
    let testTitle gol expected =
          unlines [
              "Board:"
            , show gol
            , "Should become:"
            , expected
          ]

        toStringTest gol expected =  
          it (testTitle gol expected) $ do
            golToString gol `shouldBe` expected

    context "with an empty game" $ do
      toStringTest emptyGoL ""

    context "with a glider" $ do
      toStringTest glider gliderString

  describe "stringToGoL" $ do
    let testTitle golString expected = 
          unlines [
              "String board:"
            , golString
            , "Should become:"
            , show expected
          ]

        toGolTest golString expected =
          it (testTitle golString expected) $ do
            stringToGoL golString `shouldBe` expected

    context "with an empty string" $ do
      toGolTest "" emptyGoL

    context "with a glider" $ do
      toGolTest gliderString glider

  describe "chunksOf" $ do
    let testTitle size list expected = 
          concat [
              show size
            , " | "
            , show list
            , " -> "
            , show expected
          ]

        chunkTest size list expected = 
          it (testTitle size list expected) $ do
            chunksOf size list `shouldBe` expected

    context "size 1 of []" $ do
      let size = 1
          list = [] :: [Int]
          expected = [] :: [[Int]]
        
      chunkTest size list expected

    context "size 1 of [1..3]" $ do
      let size = 1
          list = [1, 2, 3]
          expected = [[1], [2], [3]]
        
      chunkTest size list expected

    context "size 2 of [1..5]" $ do
      let size = 2
          list = [1, 2, 3, 4, 5]
          expected = [[1, 2], [3, 4], [5]]
        
      chunkTest size list expected
