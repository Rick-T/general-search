module Data.Search.Types.HeuristicCostSpec where

import Data.Monoid (Sum)
import Data.Search.Types.HeuristicCost
  ( HeuristicCost (HeuristicCost),
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property))

spec :: Spec
spec = do
  describe "Heuristic Cost" $ do
    it "constructs empty" $ do
      (mempty :: HeuristicCost (Sum Int)) `shouldBe` HeuristicCost mempty mempty
    it "adds real costs" $
      property $
        \(x, y) ->
          let HeuristicCost real _ = HeuristicCost x x <> (HeuristicCost y y :: HeuristicCost (Sum Int))
           in real == x <> y
    it "overwrites heuristic" $
      property $
        \(x, y) ->
          let HeuristicCost _ heur = HeuristicCost x x <> (HeuristicCost y y :: HeuristicCost (Sum Int))
           in heur == y
    it "sorts by combined cost" $ do
      property $
        \(x, y, h) ->
          (HeuristicCost x h < (HeuristicCost y h :: HeuristicCost (Sum Int)))
            == (x < y)
            && (HeuristicCost h x < (HeuristicCost h y :: HeuristicCost (Sum Int)))
            == (x < y)
    it "sorts by highest real cost if combined cost is equal" $ do
      property $
        \(x, y) ->
          (HeuristicCost x y < (HeuristicCost y x :: HeuristicCost (Sum Int)))
            == (x > y)