module EscSpec
  ( spec
  )
where

import           Test.Hspec
import           Control.Monad.State


newtype Queue a = Queue { items :: [a] } deriving (Show)

empty :: Queue a
empty = Queue ([] :: [a])

size :: Queue a -> Int
size = length . items

push :: a -> Queue a -> (a, Queue a)
push a q =
  let pushedItems = items q ++ [a]
      pushedQ     = Queue pushedItems
  in  (a, pushedQ)

shift :: Queue a -> (a, Queue a)
shift q =
  let shiftHelper :: Queue a -> (a, [a])
      shiftHelper (Queue []      ) = error "Cannot shift from empty queue!"
      shiftHelper (Queue (a : as)) = (a, as)
      (a, as) = shiftHelper q
  in  (a, Queue as)


spec :: Spec
spec = do
  describe "Playing with monads" $ do
    let unwrap :: Show b => Maybe b -> String
        unwrap (Just b) = show b
        unwrap Nothing  = "Nothing"

    it "Maybe" $ do
      let x     = Just 5
          y     = Just 6
          prodM = (*) <$> x <*> y
      print "prodM"
      print prodM
      print "unwrapped"
      let result = unwrap prodM
      print result
      result `shouldBe` "30"


  describe "Queue" $ do
    it "pushes" $ do
      let q0      = empty :: Queue Integer
          (_, q1) = push 1 q0
      print ("empty", q0)
      size q0 `shouldBe` 0
      print ("one item", q1)
      size q1 `shouldBe` 1

    it "shifts" $ do
      let q3 = Queue [1, 2, 3]
          (one, q2) = shift q3
      print ("full", q3)
      size q3 `shouldBe` 3
      print ("shifted", q2)
      size q2 `shouldBe` 2
      print ("shift result", one)
      one `shouldBe` 1

  describe "State Queue" $ do
    it "runs state" pending
    it "works with `do`" pending

