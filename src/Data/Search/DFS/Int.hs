{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Search using an depth-first algorithm for Int-valued nodes
module Data.Search.DFS.Int (DFS, depthFirst, dfs, module Data.Search.Types) where

import Data.IntSet (IntSet)
import Data.Search.Classes.ClosedSet as CS (ClosedSet (empty))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.DFS.Internal (Stack)
import Data.Search.Types

-- | Type synonym for the `SearchAlgorithm`
type DFS c a h = SearchAlgorithm (Stack Int c a) IntSet

-- | Create a `SearchAlgorithm` of type `DFS`. Synonym for `dfs`
depthFirst :: DFS c a h
depthFirst = SearchAlgorithm OS.empty CS.empty

-- | Create a `SearchAlgorithm` of type `DFS`. Synonym for `depthFirst`
dfs :: DFS c a h
dfs = depthFirst