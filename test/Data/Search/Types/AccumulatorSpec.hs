module Data.Search.Types.AccumulatorSpec where

import Data.Maybe (mapMaybe)
import Data.Monoid (Sum)
import Data.Search.Types.Accumulator as Acc
  ( Accumulator (getAccumulatorFunction, getEmptyAccumulator),
    contramap,
    contramapMaybe,
    filter,
    listAccumulator,
    monoidAccumulator,
    noAccumulator,
    setAccumulator,
  )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property))
import Test.QuickCheck.Function (apply)

spec :: Spec
spec = do
  describe "noAccumulator" $ do
    it "starts empty" $ do
      getEmptyAccumulator noAccumulator `shouldBe` ()
    it "does nothing" $
      property $
        \x -> accumulate (x :: [()]) noAccumulator == ()
  describe "listAccumulator" $ do
    it "starts empty" $ do
      getEmptyAccumulator listAccumulator `shouldBe` ([] :: [Void])
    it "appends elements" $
      property $
        \x -> accumulate (x :: [Int]) listAccumulator `shouldBe` x
  describe "setAccumulator" $ do
    it "starts empty" $ do
      getEmptyAccumulator setAccumulator `shouldBe` (mempty :: Set Void)
    it "inserts elements" $
      property $
        \x -> accumulate (x :: [Int]) setAccumulator `shouldBe` Set.fromList x
  describe "monoidAccumulator" $ do
    it "starts empty" $ do
      getEmptyAccumulator monoidAccumulator `shouldBe` (mempty :: Sum Int)
    it "adds elements" $
      property $
        \x -> accumulate (x :: [Sum Int]) monoidAccumulator `shouldBe` sum x
  describe "contramap" $ do
    it "maps elements" $
      property $
        \(x, fun) ->
          let f = (apply fun :: Int -> Char)
           in accumulate (x :: [Int]) (contramap f listAccumulator) `shouldBe` f <$> x
  describe "filter" $ do
    it "filters elements" $
      property $
        \(x, fun) ->
          let f = (apply fun :: Int -> Bool)
           in accumulate (x :: [Int]) (Acc.filter f listAccumulator) `shouldBe` Prelude.filter f x
  describe "contramapMaybe" $ do
    it "maps and filters elements" $
      property $
        \(x, fun) ->
          let f = (apply fun :: Int -> Maybe Char)
           in accumulate (x :: [Int]) (contramapMaybe f listAccumulator) `shouldBe` mapMaybe f x

accumulate :: [v] -> Accumulator a v -> a
accumulate elements acc = foldr (getAccumulatorFunction acc) (getEmptyAccumulator acc) elements