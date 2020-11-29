module Data.Search.Pure.GraphManip.Internal where

import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Search.BFS.Hashable
  ( Accumulator,
    breadthFirst,
  )
import Data.Search.Pure.Types.Graph (GraphP (..), getEdgesP)
import Data.Search.Pure.Types.Search (runSearchP, searchAllP)
import Data.Search.Pure.Types.SearchGoal (simpleP)

modifyEdgesP :: ((a -> f (a, c)) -> a' -> f' (a', c')) -> GraphP f c a -> GraphP f' c' a'
modifyEdgesP morph = GraphP . morph . getEdgesP
{-# INLINEABLE modifyEdgesP #-}

reduceGraphP :: (Foldable f', Hashable a, Ord a, Ord c, Monoid c, Foldable t) => (a -> (a -> f (a, c)) -> a -> f' (a, c)) -> t a -> (a -> Bool) -> Accumulator b a -> GraphP f c a -> GraphP [] (c, b) a
reduceGraphP morph domain goal accumulator graph =
  let resultsForNode a =
        let resultList = runSearchP (searchAllP (simpleP goal)) breadthFirst accumulator (modifyEdgesP (morph a) graph) a
         in [(edge, (cost, acc)) | (edge, cost, acc) <- resultList, cost > mempty]
      targets = Prelude.filter goal (toList domain)
      results = HashMap.fromList [(node, resultsForNode node) | node <- targets]
   in GraphP $ \a -> fromMaybe [] $ HashMap.lookup a results
{-# INLINEABLE reduceGraphP #-}