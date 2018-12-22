module EscSpec
  ( spec
  )
where

import Test.Hspec
import Control.Monad.State
import Control.Exception (evaluate)


newtype Queue a = Queue { items :: [a] } deriving (Show, Eq)

empty :: Queue a
empty = Queue ([] :: [a])

size :: Queue a -> Int
size = length . items

-- Non state queue

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

-- State Queue

pushS :: a -> State (Queue a) a
pushS a = state $ push a

shiftS :: State (Queue a) a
shiftS = state $ shift


--  Tests

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
      let q         = empty :: Queue Integer
          (one, q1) = push 1 q
      q1 `shouldBe` Queue [1]
      one `shouldBe` 1

    it "shifts" $ do
      let q123 = Queue [1, 2, 3]
          (one, q23) = shift q123
          (two, q3) = shift q23
      q23 `shouldBe` Queue [2, 3]
      q3 `shouldBe` Queue [3]
      one `shouldBe` 1
      two `shouldBe` 2

    it "throws an error when shifting from an empty queue" $ do
      let runsIntoError :: IO ()
          runsIntoError = do
            let x = shift (empty :: Queue Integer)
            print x

      runsIntoError `shouldThrow` errorCall "Cannot shift from empty queue!"

  describe "State Queue" $ do
    describe "pushS" $ do
      let q = empty :: Queue Integer

      it "works with `runState`" $ do
        let pusher = pushS 1
            (pushed, q1) = runState pusher q
        q1 `shouldBe` Queue [1]
        pushed `shouldBe` 1

      it "works with `do`" $ do
        let push123 = do
              pushS 1
              pushS 2
              pushS 3
            (_, q123) = runState push123 empty
        q123 `shouldBe` Queue [1, 2, 3]

    describe "shiftS" $ do
      let q123 = Queue [1, 2, 3]

      it "works with `runState`" $ do
        let (shifted, q23) = runState shiftS q123

        q23 `shouldBe` Queue [2, 3]
        shifted `shouldBe` 1

      it "works with `do`" $ do
        let shift123 :: State (Queue a) (a, a, a)
            shift123 = do
              one <- shiftS
              two <- shiftS
              three <- shiftS
              return (one, two, three)
            (shifteds, q) = runState shift123 q123
        q `shouldBe` empty
        shifteds `shouldBe` (1, 2, 3)

