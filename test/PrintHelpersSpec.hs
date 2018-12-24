module PrintHelpersSpec (spec) where

import Test.Hspec

import PrintHelpers

spec :: Spec
spec = do
  describe "linesOfWords" $ do
    let subject = linesOfWords

    context "on an array of arrays" $ do
      let input =
            [ ["See Spot run."]
            , ["Spot", "runs fast."]
            , ["Run,", "Spot,", "run."]
            ]
          expected = "See Spot run\nSpot runs fast\nRun, Spot, run."

      it "produces the expected output" $ do
        subject input `shouldBe` expected
