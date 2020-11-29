{-# OPTIONS_HADDOCK prune #-}

-- | This module contains simple default implementations of Dijkstra's algorithm, A* and DFS. Use these if you have a simple problem and don't want to worry about all the types of `Data.Search.Types.Search.Search`
module Data.Search.Simple where

import Data.Monoid (Sum, getSum)
import Data.Search.BFS.Ord
  ( bfs,
    fromSeparate,
    fromSeparateM,
    heuristic,
    heuristicM,
    listAccumulator,
    runSearch,
    runSearchT,
    search,
    searchM,
    simple,
    simpleM,
  )
import qualified Data.Search.DFS.Ord as DFS (dfs)

-- * Without monads

-- | Simple search using Dijkstra's algorithm
dijkstra ::
  (Ord a, Ord c, Num c) =>
  -- | A function pointing from one node to all it's neighbours
  (a -> [a]) ->
  -- | The cost from moving from one node to another
  (a -> a -> Maybe c) ->
  -- | The start node
  a ->
  -- | The goal
  a ->
  -- | The path from start to goal (both inclusive) and the cost. @Nothing@ if no path exists
  Maybe ([a], c)
dijkstra edges costs start goal =
  let graph = fromSeparate edges (\a b -> pure <$> costs a b)
      searchResult = runSearch (search (simple (== goal))) bfs listAccumulator graph start
   in extractResult <$> searchResult

-- | Simple search using the A* algorithm
astar ::
  (Ord a, Ord c, Num c) =>
  -- | A function pointing from one node to all it's neighbours
  (a -> [a]) ->
  -- | The cost from moving from one node to another
  (a -> a -> Maybe c) ->
  -- | The heuristic distance to the goal. Should be /consistent/ (https://en.wikipedia.org/wiki/Consistent_heuristic) otherwise the result might not be optimal
  (a -> c) ->
  -- | The start node
  a ->
  -- | The goal
  a ->
  -- | The path from start to goal (both inclusive) and the cost. @Nothing@ if no path exists
  Maybe ([a], c)
astar edges costs heur start goal =
  let graph = fromSeparate edges (\a b -> pure <$> costs a b)
      searchResult = runSearch (search (heuristic (== goal) (pure . heur))) bfs listAccumulator graph start
   in extractResult <$> searchResult

-- | Simple depth-first search
dfs ::
  (Ord a) =>
  -- | A function pointing from one node to all it's neighbours
  (a -> [a]) ->
  -- | The start node
  a ->
  -- | The goal
  a ->
  -- | A path from start to goal if one exists
  Maybe [a]
dfs edges start goal =
  let graph = fromSeparate edges (\_ _ -> Just (mempty :: Sum Int))
      searchResult = runSearch (search (simple (== goal))) DFS.dfs listAccumulator graph start
   in fst . extractResult <$> searchResult

-- * With monads

-- | Same as `dijkstra` but with the ability to use monadic functions
dijkstraM :: (Ord a, Ord c, Num c, Monad m) => (a -> m [a]) -> (a -> a -> m (Maybe c)) -> a -> a -> m (Maybe ([a], c))
dijkstraM edges costs start goal =
  let graph = fromSeparateM edges (\a b -> fmap pure <$> costs a b)
      searchResultM = runSearchT (searchM (simpleM (return . (== goal)))) bfs listAccumulator graph start
   in fmap extractResult <$> searchResultM

-- | Same as `astar` but with the ability to use monadic functions
astarM :: (Ord a, Ord c, Num c, Monad m) => (a -> m [a]) -> (a -> a -> m (Maybe c)) -> (a -> m c) -> a -> a -> m (Maybe ([a], c))
astarM edges costs heur start goal =
  let graph = fromSeparateM edges (\a b -> fmap pure <$> costs a b)
      searchResultM = runSearchT (searchM (heuristicM (return . (== goal)) (fmap pure . heur))) bfs listAccumulator graph start
   in fmap extractResult <$> searchResultM

-- | Same as `dfs` but with the ability to use monadic functions
dfsM :: (Ord a, Monad m) => (a -> m [a]) -> a -> a -> m (Maybe [a])
dfsM edges start goal =
  let graph = fromSeparateM edges (\_ _ -> return $ Just (mempty :: Sum Int))
      searchResultM = runSearchT (searchM (simpleM (return . (== goal)))) DFS.dfs listAccumulator graph start
   in fmap (fst . extractResult) <$> searchResultM

extractResult :: (a, Sum b, [n]) -> ([n], b)
extractResult (_, b, l) = (reverse l, getSum b)