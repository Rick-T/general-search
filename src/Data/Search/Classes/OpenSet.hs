{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Search.Classes.OpenSet where

import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as HashPSQ
import Data.Hashable (Hashable)
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as IntPSQ
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as OrdPSQ

-- | An `OpenSet` is a data structure containing all known nodes that have not yet been expanded.
-- The nodes are stored together with their cost and an associated value.
-- Algorithms from this packages store the value of the `Data.Search.Types.Accumulator.Accumulator`
class OpenSet q v c a | q -> v c a where
  -- | Insert a new node with the given cost
  insertWithPriority :: v -> c -> a -> q -> q

  -- | Remove the node with the lowest priority (i.e. lowest cost) from the `OpenSet`.
  -- | Return the node together with it's cost and associated value, as well as the remaining `OpenSet`
  minView :: q -> Maybe (v, c, a, q)

  -- | Create a new `OpenSet` with a single node
  singleton :: v -> c -> a -> q

  -- | Create a new empty `OpenSet`
  empty :: q

instance (Ord v, Hashable v, Ord c) => OpenSet (HashPSQ v c a) v c a where
  singleton = HashPSQ.singleton
  minView = HashPSQ.minView
  insertWithPriority node prio acc queue = case HashPSQ.lookup node queue of
    Nothing -> HashPSQ.insert node prio acc queue
    Just (prio', _) -> if prio' <= prio then queue else HashPSQ.insert node prio acc queue
  empty = HashPSQ.empty

instance (Ord v, Ord c) => OpenSet (OrdPSQ v c a) v c a where
  singleton = OrdPSQ.singleton
  minView = OrdPSQ.minView
  insertWithPriority node prio acc queue = case OrdPSQ.lookup node queue of
    Nothing -> OrdPSQ.insert node prio acc queue
    Just (prio', _) -> if prio' <= prio then queue else OrdPSQ.insert node prio acc queue
  empty = OrdPSQ.empty

instance (Ord c) => OpenSet (IntPSQ c a) Int c a where
  singleton = IntPSQ.singleton
  minView = IntPSQ.minView
  insertWithPriority node prio acc queue = case IntPSQ.lookup node queue of
    Nothing -> IntPSQ.insert node prio acc queue
    Just (prio', _) -> if prio' <= prio then queue else IntPSQ.insert node prio acc queue
  empty = IntPSQ.empty