module Data.Search.Types.GraphSpec where

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Monoid (Sum)
import Data.Search.Types.Graph
  ( fromCombined,
    fromCombinedM,
    fromSeparate,
    fromSeparateM,
    getEdges,
    getEdgesM,
  )
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Testable (property))
import Test.QuickCheck.Function (apply, applyFun2)

spec :: Spec
spec = do
  describe "Graph" $ do
    it "constructs from combined" $
      property $
        \(input, edgesFun) ->
          let edges = (apply edgesFun :: Int -> [(Int, Sum Float)])
           in getEdges (fromCombined edges) (input :: Int) == edges input
    it "constructs from separate" $
      property $
        \(input, edgesFun, costFun) ->
          let edges = (apply edgesFun :: Int -> [Int])
              cost = (applyFun2 costFun :: Int -> Int -> Maybe (Sum Float))
              graphEdges input = getEdges (fromSeparate edges cost) (input :: Int)
           in all (\(node, value) -> node `elem` edges input && value == fromJust (cost input node)) $ graphEdges input
    it "constructs from combined (monadic)" $
      property $
        \(input, edgesFun) ->
          let edges = (apply edgesFun :: Int -> Maybe [(Int, Sum Float)])
           in getEdgesM (fromCombinedM edges) (input :: Int) == edges input
    it "constructs from separate (monadic)" $
      property $
        \(input, edgesFun, costFun) ->
          let edges = (apply edgesFun :: Int -> Maybe [Int])
              cost = (applyFun2 costFun :: Int -> Int -> Maybe (Maybe (Sum Float)))
              graphEdges input = getEdgesM (fromSeparateM edges cost) (input :: Int)
           in case edges input of
                Nothing -> isNothing $ graphEdges input
                Just nodes ->
                  if not $ all (isJust . cost input) nodes
                    then isNothing $ graphEdges input
                    else all (\(n, c) -> c == fromJust (fromJust (cost input n))) (fromJust $ graphEdges input)