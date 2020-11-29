{-# LANGUAGE FlexibleContexts #-}

module Data.Search.Types.Search
  ( -- * Data types
    Search,
    SearchT,

    -- * Defining a search
    search,
    searchM,
    searchStep,
    searchStepM,
    searchMany,
    searchManyM,
    searchAll,
    searchAllM,

    -- * Executing a search
    runSearch,
    runSearchT,
  )
where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State
  ( evalStateT,
  )
import Data.Search.Classes.ClosedSet as CS (ClosedSet (..))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.Types.Accumulator
  ( Accumulator (getAccumulatorFunction, getEmptyAccumulator),
  )
import Data.Search.Types.Graph
  ( Graph,
    GraphM,
  )
import Data.Search.Types.HeuristicCost
  ( HeuristicCost,
  )
import Data.Search.Types.Search.Internal
  ( Search,
    SearchStateM (SearchStateM),
    SearchT (getStateT),
    StepResult,
    searchAllG,
    searchG,
    searchManyG,
    searchStepG,
    updateOpenSet,
    updateOpenSetM,
  )
import Data.Search.Types.SearchAlgorithm (SearchAlgorithm)
import Data.Search.Types.SearchGoal (SearchGoal, SearchGoalM)

-- | Run the search algorithm until it finds a node that matches the given `SearchGoal`
search ::
  (Semigroup c, OpenSet q n (HeuristicCost c) a, Foldable f, ClosedSet h n) =>
  -- | The goal of the search
  SearchGoal c n ->
  -- | The resulting node together with the cost of reaching it and the accumulated value of nodes along the path
  Search n c a f q h (Maybe (n, c, a))
search = searchG searchStep
{-# INLINEABLE search #-}

-- | Like `search` but with the possibility of using effects from the inner monad
searchM :: (Semigroup c, Monad m, OpenSet q n (HeuristicCost c) a, Foldable f, ClosedSet h n) => SearchGoalM m c n -> SearchT n c a f q h m (Maybe (n, c, a))
searchM = searchG searchStepM
{-# INLINEABLE searchM #-}

-- | Execute a single step of the search algorithm and report if the goal was found
searchStep ::
  (Foldable f, ClosedSet h n, OpenSet q n (HeuristicCost c) a, Semigroup c) =>
  -- | The goal of the search
  SearchGoal c n ->
  -- | The `StepResult` containing the expanded node if the search has not exhausted all nodes
  Search n c a f q h (StepResult n c a)
searchStep = searchStepG updateOpenSet
{-# INLINE searchStep #-}

-- | Like `searchStep` but with the possibility of using effects from the inner monad
searchStepM :: (Semigroup c, Monad m, OpenSet q n (HeuristicCost c) a, Foldable f, ClosedSet h n) => SearchGoalM m c n -> SearchT n c a f q h m (StepResult n c a)
searchStepM = searchStepG updateOpenSetM
{-# INLINE searchStepM #-}

-- | Search many nodes that match the goal
searchMany ::
  (Semigroup c, OpenSet q n (HeuristicCost c) a, Foldable f, ClosedSet h n) =>
  -- | The number of goal nodes that should be found (@n@)
  Int ->
  -- | The goal of the search
  SearchGoal c n ->
  -- | The first @n@ goal nodes together with their cost and their accumulated values, ordered by cost. Might be less than @n@ if not enough nodes are reachable from the start
  Search n c a f q h [(n, c, a)]
searchMany = searchManyG search
{-# INLINE searchMany #-}

-- | Like `searchMany` but with the possibility of using effects from the inner monad
searchManyM :: (Monad m, Semigroup c, OpenSet q n (HeuristicCost c) a, Foldable f, ClosedSet h n) => Int -> SearchGoalM m c n -> SearchT n c a f q h m [(n, c, a)]
searchManyM = searchManyG searchM
{-# INLINE searchManyM #-}

-- | Like `searchMany` but find all possible goals that are reachable
searchAll :: (Semigroup c, OpenSet q n (HeuristicCost c) a, ClosedSet h n, Foldable f) => SearchGoal c n -> Search n c a f q h [(n, c, a)]
searchAll = searchAllG search
{-# INLINE searchAll #-}

-- | Like `searchManyM` but find all possible goals that are reachable
searchAllM :: (Monad m, Semigroup c, OpenSet q n (HeuristicCost c) a, ClosedSet h n, Foldable f) => SearchGoalM m c n -> SearchT n c a f q h m [(n, c, a)]
searchAllM = searchAllG searchM
{-# INLINE searchAllM #-}

-- | Run the given search and return its result
runSearch ::
  (OpenSet q n (HeuristicCost c) a, ClosedSet h n, Monoid c) =>
  -- | The `Search` defining what to search and what to return
  Search n c a f q h r ->
  -- | The `SearchAlgorithm` that should be used
  SearchAlgorithm q h ->
  -- | The `Accumulator` that should be used
  Accumulator a n ->
  -- | The `Graph` representing the connections and between the nodes
  Graph f c n ->
  -- | The starting node.
  n ->
  -- | The result of the search
  r
runSearch srch alg acc graph start = runIdentity $ runSearchT srch alg acc graph start
{-# INLINEABLE runSearch #-}

-- | Like `runSearch` but with the possibility of using effects from the inner monad. Returns the inner monad.
runSearchT :: (Monad m, OpenSet q n (HeuristicCost c) a, ClosedSet h n, Monoid c) => SearchT n c a f q h m r -> SearchAlgorithm q h -> Accumulator a n -> GraphM m f c n -> n -> m r
runSearchT srch _ acc graph start =
  let closedSet = CS.empty
      openSet = singleton start mempty $ getEmptyAccumulator acc
   in evalStateT (getStateT srch) $ SearchStateM openSet closedSet graph $ getAccumulatorFunction acc
{-# INLINE runSearchT #-}