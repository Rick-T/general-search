-- | This module contains different functions to manipulate `Graph`s
module Data.Search.GraphManip (connectAll, connectAll', connectAllM, connectAllM', filterEdges, filterEdges', filterEdgesM, filterEdgesM', modifyEdges, modifyEdgesM) where

import Data.Foldable (Foldable (toList))
import Data.Hashable (Hashable)
import Data.Search.BFS.Hashable
  ( Accumulator,
    Graph,
    GraphM,
    noAccumulator,
  )
import Data.Search.GraphManip.Internal
  ( dropAccumulator,
    dropAccumulatorM,
    modifyEdges,
    modifyEdgesM,
    reduceGraph,
    reduceGraphM,
  )

-- * Connecting nodes

-- | Fully connect all nodes of a `Graph`.
--
-- Every node will be connected to every other node.
-- The cost between them will be the cost of the shortest path between them.
connectAll ::
  (Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c) =>
  -- | All the nodes that should be used for the computation
  t v ->
  -- | All nodes that satisfy this predicate will remain in the `Graph`
  (v -> Bool) ->
  -- | The `Graph` that will be modified
  Graph f c v ->
  -- | The resulting `Graph`
  Graph [] c v
connectAll domain goal graph = modifyEdges dropAccumulator $ connectAll' domain goal noAccumulator graph
{-# INLINEABLE connectAll #-}

-- | Like `connectAll` but the edges will not only be labelled with the cost between to nodes but also with the accumulated value from the given `Accumulator`.
-- | The accumulated value for a pair of nodes will be calculated from all the nodes on the shortest path between them.
connectAll' :: (Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c) => t v -> (v -> Bool) -> Accumulator a v -> Graph f c v -> Graph [] (c, a) v
connectAll' = reduceGraph $ const id
{-# INLINEABLE connectAll' #-}

-- | Like `connectAll` but with the possibility to use monadic effects during the computation
connectAllM :: (Monad m, Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c) => t v -> (v -> m Bool) -> GraphM m f c v -> GraphM m [] c v
connectAllM domain goal graph = modifyEdgesM dropAccumulatorM $ connectAllM' domain goal noAccumulator graph
{-# INLINEABLE connectAllM #-}

-- | Like `connectAll'` but with the possibility to use monadic effects during the computation
connectAllM' :: (Monad m, Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c) => t v -> (v -> m Bool) -> Accumulator a v -> GraphM m f c v -> GraphM m [] (c, a) v
connectAllM' = reduceGraphM $ const id
{-# INLINEABLE connectAllM' #-}

-- * Contracting nodes

-- | Remove nodes from a `Graph` by contracting their edges.
--
-- Every node not satisfying the predicate will be removed from the `Graph`. All edges entering or leaving that node will be contracted.
filterEdges ::
  (Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c) =>
  -- | All the nodes that should be used for the computation
  t v ->
  -- | All nodes that satisfy this predicate will remain in the `Graph`
  (v -> Bool) ->
  -- | The `Graph` that will be modified
  Graph f c v ->
  -- | The resulting `Graph`
  Graph [] c v
filterEdges domain goal graph = modifyEdges dropAccumulator $ filterEdges' domain goal noAccumulator graph
{-# INLINEABLE filterEdges #-}

-- | Like `filterEdges` but the edges will not only be labelled with the cost between to nodes but also with the accumulated value from the given `Accumulator`.
-- | The accumulated value for a pair of nodes will be calculated from all the nodes on the shortest path between them.
filterEdges' :: (Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c) => t v -> (v -> Bool) -> Accumulator a v -> Graph f c v -> Graph [] (c, a) v
filterEdges' domain goal = reduceGraph (\start edges a -> if a /= start && goal a then [] else toList $ edges a) domain goal
{-# INLINEABLE filterEdges' #-}

-- | Like `filterEdges` but with the possibility to use monadic effects during the computation
filterEdgesM :: (Monad m, Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c) => t v -> (v -> m Bool) -> GraphM m f c v -> GraphM m [] c v
filterEdgesM domain goal graph = modifyEdgesM dropAccumulatorM $ filterEdgesM' domain goal noAccumulator graph
{-# INLINEABLE filterEdgesM #-}

-- | Like `filterEdges'` but with the possibility to use monadic effects during the computation
filterEdgesM' :: (Monad m, Foldable t, Foldable f, Hashable v, Ord v, Ord c, Monoid c) => t v -> (v -> m Bool) -> Accumulator a v -> GraphM m f c v -> GraphM m [] (c, a) v
filterEdgesM' domain goal =
  reduceGraphM
    ( \start edges a -> do
        matchesGoal <- goal a
        if a /= start && matchesGoal
          then return []
          else toList <$> edges a
    )
    domain
    goal
{-# INLINEABLE filterEdgesM' #-}