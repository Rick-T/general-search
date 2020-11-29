{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home, ignore-exports #-}

module Data.Search.Types.Search.Internal where

import Control.Monad (replicateM)
import Control.Monad.Identity (Identity)
import Control.Monad.State
  ( MonadState (get),
    MonadTrans,
    StateT (StateT),
    lift,
    modify,
  )
import Data.Foldable (foldrM)
import Data.Maybe (catMaybes)
import Data.Search.Classes.ClosedSet as CS (ClosedSet (..))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.Types.Graph
  ( Graph,
    GraphM,
    getEdges,
    getEdgesM,
  )
import Data.Search.Types.HeuristicCost
  ( HeuristicCost (HeuristicCost, getRealCost),
  )
import Data.Search.Types.SearchGoal
  ( SearchGoal,
    SearchGoalM,
    getHeuristic,
    getHeuristicM,
    isGoalM,
  )

-- * Data types

-- | A parameterized search monad.
--
-- @n@ is the type of nodes that are going to be searched. In general arbitrary node-types can be used. However a concrete search algorithm generally will place constraints on them (e.g. @Ord n@ oder @Hashable n@).
--
-- @c@ is the type of the costs going from on node to another. A search is possible for all ordered monoids.
--
-- @a@ is the type of `Accumulator` that is used to accumulate a value along the paths. E.g a `Data.Search.Types.Accumulator.listAccumulator` can be used to trace the path from goal to start.
--
-- @f@ is the type of foldable that contains the all the neighbours of a node.
--
-- @q@ is the type of the `Data.Search.Classes.OpenSet.OpenSet` that stores the known nodes.
--
-- @h@ is the type of the `Data.Search.Classes.ClosedSet.ClosedSet` that stores the expanded nodes.
--
-- @r@ is the return type of the computation
--
-- `Search` represents executing a search algorithm from a (fixed) beginning node.
--
-- `return` will leave the search-state unchanged.
--
-- @>>=@ will continue the algorithm from the state of the previous computation.
-- This can be used to find multiple paths to different goals.
--
-- __NOTE__: In many search algorithms every node will be expanded only once. That means generally it is not possible to backtrack.
-- In that case this monad is not commutative.
-- If the path /A-C/ is shorter than /A-B/, then @search B *> search C@ will only find the path /A-B/, while @search C *> search B@ will find both /A-B/ and /A-C/.
type Search n c a f q h r = SearchT n c a f q h Identity r

-- | A parameterized search monad transformer.
--
-- Like `Search` but with an additional parameter @m@ which is the type of the inner monad.
newtype SearchT n c a f q h m r = SearchT {getStateT :: StateT (SearchStateM n c a f q h m) m r} deriving (Functor, Applicative, Monad, MonadState (SearchStateM n c a f q h m))

-- | The data type representing the current state of the algorithm
data SearchStateM n c a f q h m = SearchStateM
  { -- | The state of the `Data.Search.Classes.OpenSet.OpenSet`
    openSetState :: q,
    -- | The state of the `Data.Search.Classes.ClosedSet.ClosedSet`
    closedSetState :: h,
    -- | The `Data.Search.Types.Graph.Graph` that is searched
    graphState :: GraphM m f c n,
    -- | The function for adding elements to the `Accumulator`
    accumulatorFunctionState :: n -> a -> a
  }

-- | Result of a single step in the search algorithm
data StepResult n c a
  = -- | There are no more known nodes left to expand
    EXHAUSTED
  | -- | We have expanded some node that was not our goal
    EXPANDED_NODE (n, c, a)
  | -- | We have expanded a node that was our goal
    EXPANDED_GOAL (n, c, a)

-- * Instances

instance MonadTrans (SearchT n c a f q h) where
  lift m = SearchT $ lift m
  {-# INLINEABLE lift #-}

-- | A general search that iterates the given function until the goal was found
searchG ::
  (Semigroup c, Monad m, OpenSet q n (HeuristicCost c) a, Foldable f, ClosedSet h n) =>
  -- | Function that executes one search-step and reports if the goal was found
  (SearchGoalM m c n -> SearchT n c a f q h m (StepResult n c a)) ->
  -- | The goal of the search
  SearchGoalM m c n ->
  -- | The resulting node together with the cost of reaching it and the accumulated value of nodes along the path
  SearchT n c a f q h m (Maybe (n, c, a))
searchG stepFunc target =
  let searchG' target' = do
        result <- stepFunc target'
        case result of
          EXPANDED_NODE _ -> searchG' target'
          EXPANDED_GOAL goal -> return $ Just goal
          EXHAUSTED -> return Nothing
   in searchG' target
{-# INLINE searchG #-}

-- | A general implementation of a search algorithm that uses the given function to update the `Data.Search.Classes.OpenSet.OpenSet`
searchStepG ::
  (Semigroup c, Monad m, OpenSet q n (HeuristicCost c) a, Foldable f, ClosedSet h n) =>
  -- | This function is used to update the `Data.Search.Classes.OpenSet.OpenSet`
  (q -> h -> SearchGoalM m c n -> GraphM m f c n -> n -> a -> HeuristicCost c -> SearchT n c a f q h m q) ->
  -- | The goal of the search
  SearchGoalM m c n ->
  -- | The `StepResult` containing the expanded node if the search has not exhausted all nodes
  SearchT n c a f q h m (StepResult n c a)
searchStepG updateOpenSetG target = do
  (SearchStateM q v graph append) <- get
  case minView q of
    Nothing -> return EXHAUSTED
    Just (a, value, acc, q') -> do
      let acc' = append a acc
      let v' = insert a v
      q'' <- updateOpenSetG q' v target graph a acc' value
      modify $ \s -> s {openSetState = q'', closedSetState = v'}
      goal <- lift $ isGoalM target a
      if goal then return $ EXPANDED_GOAL (a, getRealCost value, acc') else return $ EXPANDED_NODE (a, getRealCost value, acc')
{-# INLINE searchStepG #-}

-- | Search many nodes that match the goal using the given search function
searchManyG ::
  (Monad m, Semigroup c, OpenSet q n (HeuristicCost c) a, Foldable f, ClosedSet h n) =>
  -- | Function for search the goal nodes
  (SearchGoalM m c n -> SearchT n c a f q h m (Maybe (n, c, a))) ->
  -- | The number of goal nodes that should be found (@n@)
  Int ->
  -- | The goal of the search
  SearchGoalM m c n ->
  -- | The first @n@ goal nodes together with their cost and their accumulated values, ordered by cost. Might be less than @n@ if not enough nodes are reachable from the start
  SearchT n c a f q h m [(n, c, a)]
searchManyG searchFunc n goal = catMaybes <$> replicateM n (searchFunc goal)
{-# INLINE searchManyG #-}

-- | Like `searchManyG` but find all possible goals that are reachable
searchAllG :: Monad m => (p -> m (Maybe n)) -> p -> m [n]
searchAllG searchFunc goal =
  let searchAllG' =
        searchFunc goal >>= \mResult -> case mResult of
          Nothing -> return []
          Just result -> do
            next <- searchAllG'
            return $ result : next
   in searchAllG'
{-# INLINE searchAllG #-}

-- * Utility functions

-- | Update the `Data.Search.Classes.OpenSet.OpenSet` for searches without an inner monad (or rather with `Identity`).
-- In this case the update can be done a bit faster than with the more general `updateOpenSetM`
updateOpenSet :: (Foldable f, ClosedSet h n, OpenSet q n (HeuristicCost c) a, Semigroup c) => q -> h -> SearchGoal c n -> Graph f c n -> n -> a -> HeuristicCost c -> Search n c a f q h q
updateOpenSet q' v' target graph a acc' value = return (foldr (\(neighbour, realCost) queue -> if neighbour `member` v' then queue else insertWithPriority neighbour (value <> HeuristicCost realCost (getHeuristic target neighbour)) acc' queue) q' $ getEdges graph a)
{-# INLINE updateOpenSet #-}

-- | Update the `Data.Search.Classes.OpenSet.OpenSet` for searches with an arbitraty inner monad.
updateOpenSetM :: (Foldable f, Monad m, OpenSet q n (HeuristicCost c) a, Semigroup c, ClosedSet h n) => q -> h -> SearchGoalM m c n -> GraphM m f c n -> n -> a -> HeuristicCost c -> SearchT n c a f q h m q
updateOpenSetM q' v' target graph a acc' value = lift $
  do
    edges <- getEdgesM graph a
    foldrM
      ( \(neighbour, realCost) queue ->
          if neighbour `member` v'
            then return queue
            else do
              heuristic <- getHeuristicM target neighbour
              return $ insertWithPriority neighbour (value <> HeuristicCost realCost heuristic) acc' queue
      )
      q'
      edges
{-# INLINE updateOpenSetM #-}
