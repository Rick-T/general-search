{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Search using an depth-first algorithm for nodes implementing `Ord`
module Data.Search.DFS.Ord (DFS, depthFirst, dfs, module Data.Search.Types) where

import Data.Search.Classes.ClosedSet as CS (ClosedSet (empty))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.DFS.Internal (Stack)
import Data.Search.Types
import Data.Set (Set)

-- | Type synonym for the `SearchAlgorithm`
type DFS v c a h = SearchAlgorithm (Stack v c a) (Set v)

-- | Create a `SearchAlgorithm` of type `DFS`. Synonym for `dfs`
depthFirst :: (Ord v) => DFS v c a h
depthFirst = SearchAlgorithm OS.empty CS.empty

-- | Create a `SearchAlgorithm` of type `DFS`. Synonym for `depthFirst`
dfs :: (Ord v) => DFS v c a h
dfs = depthFirst