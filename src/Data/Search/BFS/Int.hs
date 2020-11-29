{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Search using an breadth-first `SearchAlgorithm` (A* or Dijkstra) for Int-valued nodes
module Data.Search.BFS.Int (BFS, breadthFirst, bfs, module Data.Search.Types) where

import Data.IntPSQ (IntPSQ)
import Data.IntSet (IntSet)
import Data.Search.Classes.ClosedSet as CS (ClosedSet (empty))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.Types

-- | Type synonym for the `SearchAlgorithm`
type BFS c a = SearchAlgorithm (IntPSQ c a) IntSet

-- | Create a `SearchAlgorithm` of type `BFS`. Synonym for `bfs`
breadthFirst :: Ord c => BFS c a
breadthFirst = SearchAlgorithm OS.empty CS.empty

-- | Create a `SearchAlgorithm` of type `BFS`. Synonym for `breadthFirst`
bfs :: Ord c => BFS c a
bfs = breadthFirst