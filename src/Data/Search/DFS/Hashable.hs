{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Search using an depth-first `SearchAlgorithm` for `Hashable` nodes
module Data.Search.DFS.Hashable (DFS, depthFirst, dfs, module Data.Search.Types) where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Search.Classes.ClosedSet as CS (ClosedSet (empty))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.DFS.Internal (Stack)
import Data.Search.Types

-- | Type synonym for the `SearchAlgorithm`
type DFS v c a h = SearchAlgorithm (Stack v c a) (HashSet v)

-- | Create a `SearchAlgorithm` of type `DFS`. Synonym for `dfs`
depthFirst :: (Eq v, Hashable v) => DFS v c a h
depthFirst = SearchAlgorithm OS.empty CS.empty

-- | Create a `SearchAlgorithm` of type `DFS`. Synonym for `depthFirst`
dfs :: (Eq v, Hashable v) => DFS v c a h
dfs = depthFirst