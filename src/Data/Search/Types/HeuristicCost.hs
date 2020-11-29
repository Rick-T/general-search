module Data.Search.Types.HeuristicCost where

-- | Data type for associating nodes with a real cost as well as a heuristic.
-- This is used in the A* algorithm to prioritise nodes with low heursitic costs.
data HeuristicCost c = HeuristicCost
  { getRealCost :: c,
    getHeuristicCost :: c
  }
  deriving (Eq, Show)

instance (Semigroup c, Ord c) => Ord (HeuristicCost c) where
  (HeuristicCost a b) `compare` (HeuristicCost x y) = case (a <> b) `compare` (x <> y) of
    LT -> LT
    GT -> GT
    EQ -> x `compare` a -- If the estimated cost of two nodes is the same, prioritise the one with the higher real cost

instance Semigroup c => Semigroup (HeuristicCost c) where
  (HeuristicCost a _) <> (HeuristicCost x y) = HeuristicCost (a <> x) y

instance Monoid c => Monoid (HeuristicCost c) where
  mempty = HeuristicCost mempty mempty
