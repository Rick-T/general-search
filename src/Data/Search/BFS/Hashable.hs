{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Search using an breadth-first `SearchAlgorithm` (A* or Dijkstra) for nodes implementing `Data.Hashable.Hashable`
module Data.Search.BFS.Hashable (BFS, breadthFirst, bfs, module Data.Search.Types) where

import Data.HashPSQ (HashPSQ)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Search.Classes.ClosedSet as CS (ClosedSet (empty))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.Types

-- | Type synonym for the `SearchAlgorithm`
type BFS v c a = SearchAlgorithm (HashPSQ v c a) (HashSet v)

-- | Create a `SearchAlgorithm` of type `BFS`. Synonym for `bfs`
breadthFirst :: (Ord c, Hashable v, Ord v) => BFS v c a
breadthFirst = SearchAlgorithm OS.empty CS.empty

-- | Create a `SearchAlgorithm` of type `BFS`. Synonym for `breadthFirst`
bfs :: (Hashable v, Ord c, Ord v) => BFS v c a
bfs = breadthFirst