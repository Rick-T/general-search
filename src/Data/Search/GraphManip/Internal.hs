module Data.Search.GraphManip.Internal where

import Control.Monad (filterM)
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Search.BFS.Hashable
  ( Accumulator,
    Graph,
    GraphM (..),
    breadthFirst,
    getEdges,
    runSearch,
    runSearchT,
    searchAll,
    searchAllM,
    simple,
    simpleM,
  )

modifyEdges :: ((v -> f (v, c)) -> v' -> f' (v', c')) -> Graph f c v -> Graph f' c' v'
modifyEdges morph graph = GraphM $ return . morph (getEdges graph)
{-# INLINEABLE modifyEdges #-}

modifyEdgesM :: ((v -> m (f (v, c))) -> v' -> m (f' (v', c'))) -> GraphM m f c v -> GraphM m f' c' v'
modifyEdgesM morph (GraphM edges) = GraphM $ \a -> morph edges a
{-# INLINEABLE modifyEdgesM #-}

dropAccumulator :: (v -> [(v, (c, ()))]) -> v -> [(v, c)]
dropAccumulator edges a = [(edge, cost) | (edge, (cost, _)) <- edges a]
{-# INLINEABLE dropAccumulator #-}

dropAccumulatorM :: Monad m => (v -> m [(v, (c, ()))]) -> v -> m [(v, c)]
dropAccumulatorM edges a = do
  edgeList <- edges a
  return [(edge, cost) | (edge, (cost, _)) <- edgeList]
{-# INLINEABLE dropAccumulatorM #-}

reduceGraph :: (Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c, Foldable f') => (v -> (v -> f (v, c)) -> v -> f' (v, c)) -> t v -> (v -> Bool) -> Accumulator a v -> Graph f c v -> Graph [] (c, a) v
reduceGraph morph domain goal accumulator graph =
  let resultsForNode a =
        let resultList = runSearch (searchAll (simple goal)) breadthFirst accumulator (modifyEdges (morph a) graph) a
         in [(edge, (cost, acc)) | (edge, cost, acc) <- resultList, cost > mempty]
      targets = Prelude.filter goal (toList domain)
      results = HashMap.fromList [(node, resultsForNode node) | node <- targets]
   in GraphM $ \a -> return $ fromMaybe [] $ HashMap.lookup a results
{-# INLINEABLE reduceGraph #-}

reduceGraphM :: (Monad m, Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c, Foldable f') => (v -> (v -> m (f (v, c))) -> v -> m (f' (v, c))) -> t v -> (v -> m Bool) -> Accumulator a v -> GraphM m f c v -> GraphM m [] (c, a) v
reduceGraphM morph domain goal accumulator graph =
  let resultsForNode a = do
        resultList <- runSearchT (searchAllM (simpleM goal)) breadthFirst accumulator (modifyEdgesM (morph a) graph) a
        return [(edge, (cost, acc)) | (edge, cost, acc) <- resultList, cost > mempty]
      targetsM = filterM goal (toList domain)
      resultsM = do
        targets <- targetsM
        return $ HashMap.fromList [(node, resultsForNode node) | node <- targets]
   in GraphM $ \a -> do
        results <- resultsM
        fromMaybe (return []) $ HashMap.lookup a results
{-# INLINEABLE reduceGraphM #-}