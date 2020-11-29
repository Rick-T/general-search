module Data.Search.GraphManipSpec where

import Data.Monoid (Sum)
import Data.Search.ExampleGraph
  ( City (F, KL, LU, WÜ),
    directedGraph,
  )
import Data.Search.GraphManip (connectAll, filterEdges)
import Data.Search.Types.Graph (Graph, getEdges)
import Test.Hspec (Spec, describe, it, shouldBe)

withoutLU :: Graph [] (Sum Int) City
withoutLU = filterEdges [minBound .. maxBound] (/= LU) directedGraph

fullyConnected :: Graph [] (Sum Int) City
fullyConnected = connectAll [minBound .. maxBound] (const True) directedGraph

spec :: Spec
spec = do
  describe "filterEdges" $ do
    it "should contract vertices" $ do
      getEdges withoutLU KL `shouldBe` [(F, return 103), (WÜ, return (53 + 183))]
  describe "connectAll" $ do
    it "should find shortes path to all reachable noes" $ do
      getEdges fullyConnected KL `shouldBe` [(LU, return 53), (F, return 103), (WÜ, return (103 + 116))]