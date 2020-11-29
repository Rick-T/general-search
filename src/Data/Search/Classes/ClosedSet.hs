{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Search.Classes.ClosedSet where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Set (Set)
import qualified Data.Set as Set

-- | A `ClosedSet` is a data structure where fully expanded nodes are stored.
class ClosedSet s e | s -> e where
  -- | Insert an expanded node into the `ClosedSet`
  insert :: e -> s -> s

  -- | Check if a node is already present in the `ClosedSet`
  member :: e -> s -> Bool

  -- | Create a new empty `ClosedSet`
  empty :: s

instance (Eq e, Hashable e) => ClosedSet (HashSet e) e where
  insert = HashSet.insert
  member = HashSet.member
  empty = mempty

instance (Eq e, Ord e) => ClosedSet (Set e) e where
  insert = Set.insert
  member = Set.member
  empty = mempty

instance ClosedSet IntSet Int where
  insert = IntSet.insert
  member = IntSet.member
  empty = mempty