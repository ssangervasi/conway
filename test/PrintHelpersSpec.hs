module PrintHelpersSpec (spec) where

import Test.Hspec

import PrintHelpers

spec :: Spec
spec = do
  describe "quote" $ do
    it "wraps a string in quotes" $ do
      quote "Say it ain't so!" `shouldBe`
            "\"Say it ain't so!\""

  describe "linesOfWords" $ do
    context "on an array of arrays" $ do
      it "produces many lines of words" $ do
        linesOfWords seeSpotList `shouldBe` seeSpotString

  describe "wordsOfLines" $ do
    context "on many lines of words" $ do
      it "produces an array of arrays" $ do
        wordsOfLines seeSpotString `shouldBe` seeSpotList


seeSpotString = "See Spot run.\nSpot runs fast.\nRun, Spot, run.\n"
seeSpotList =
  [ ["See" , "Spot" , "run." ]
  , ["Spot", "runs" , "fast."]
  , ["Run,", "Spot,", "run." ]
  ]