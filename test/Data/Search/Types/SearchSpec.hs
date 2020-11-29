module Data.Search.Types.SearchSpec where

import Data.Search.BFS.Hashable
  ( bfs,
    listAccumulator,
    runSearch,
    search,
    searchAll,
  )
import Data.Search.DFS.Hashable
  ( dfs,
  )
import Data.Search.ExampleGraph
  ( City (F, HN, KA, KL, LU, SB, WÜ),
    directedGraph,
    ludwigshafen,
    undirectedGraph,
    würzburg,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "BFS" $ do
    it "should find minimum path (directed)" $
      let Just (_, cost, rpath) = runSearch (search würzburg) bfs listAccumulator directedGraph SB
       in do
            cost `shouldBe` return (70 + 103 + 116)
            reverse rpath `shouldBe` [SB, KL, F, WÜ]
    it "should find minimum path (undirected)" $
      let Just (_, cost, rpath) = runSearch (search würzburg) bfs listAccumulator undirectedGraph SB
       in do
            cost `shouldBe` return (70 + 103 + 116)
            reverse rpath `shouldBe` [SB, KL, F, WÜ]
    it "can find multiple targets" $
      let results = runSearch (searchAll (würzburg <> ludwigshafen)) bfs listAccumulator undirectedGraph SB
       in do
            (\(_, _, rpath) -> reverse rpath) <$> results
              `shouldBe` [ [SB, KL, LU],
                           [SB, KL, F, WÜ]
                         ]
            (\(_, cost, _) -> cost) <$> results
              `shouldBe` [ return (70 + 53),
                           return (70 + 103 + 116)
                         ]
  describe "DFS" $ do
    it "should find some path (directed)" $
      let Just (_, cost, rpath) = runSearch (search ludwigshafen) dfs listAccumulator directedGraph SB
       in do
            cost `shouldBe` return (70 + 53)
            reverse rpath `shouldBe` [SB, KL, LU]
    it "should find some path (undirected)" $
      let Just (_, cost, rpath) = runSearch (search ludwigshafen) dfs listAccumulator undirectedGraph SB
       in do
            cost `shouldBe` return (145 + 84 + 102 + 183)
            reverse rpath `shouldBe` [SB, KA, HN, WÜ, LU]