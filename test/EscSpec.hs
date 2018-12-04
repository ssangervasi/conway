module EscSpec (spec) where

import Test.Hspec

puts = putStrLn

unwrap :: Show b => Maybe b -> String
unwrap (Just b) = show b
unwrap Nothing = "Nothing"

spec :: Spec
spec = do
  describe "Playing with monads" $ do
    it "Maybe" $ do
      let x = Just 5
          y = Just 6
          prodM = (*) <$> x <*> y
      puts "prodM"
      puts $ show $ prodM
      puts "unwrapped"
      puts $ unwrap prodM

