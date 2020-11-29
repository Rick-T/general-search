{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Search using an breadth-first `SearchAlgorithm` (A* or Dijkstra) for nodes implementing `Ord`
module Data.Search.BFS.Ord (BFS, breadthFirst, bfs, module Data.Search.Types) where

import Data.OrdPSQ (OrdPSQ)
import Data.Search.Classes.ClosedSet as CS (ClosedSet (empty))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.Types
import Data.Set (Set)

-- | Type synonym for the `SearchAlgorithm`
type BFS v c a = SearchAlgorithm (OrdPSQ v c a) (Set v)

-- | Create a `SearchAlgorithm` of type `BFS`. Synonym for `bfs`
breadthFirst :: (Ord v, Ord c) => BFS v c a
breadthFirst = SearchAlgorithm OS.empty CS.empty

-- | Create a `SearchAlgorithm` of type `BFS`. Synonym for `breadthFirst`
bfs :: (Ord v, Ord c) => BFS v c a
bfs = breadthFirst