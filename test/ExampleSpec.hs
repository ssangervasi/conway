module ExampleSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "A function" $ do
    let subject () = True

    context "on a kind of input" $ do
      let input = ()
          expected = True

      it "produces the expected output" $ do
        subject input `shouldBe` expected
