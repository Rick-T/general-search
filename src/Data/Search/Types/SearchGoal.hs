module Data.Search.Types.SearchGoal where

import Control.Monad (liftM2)
import Control.Monad.Identity (Identity, runIdentity)

-- | A `SearchGoal` defines the goal of a `Data.Search.Types.Search.Search`.
--
-- @c@ is the type of the the heuristic cost from a given node to the goal.
-- The type has to match the type of the cost for moving between `Data.Search.Types.Graph.Graph`-nodes.
--
-- @n@ is the type of the `Data.Search.Types.Graph.Graph`-nodes.
type SearchGoal c n = SearchGoalM Identity c n

-- | Like `SearchGoal` but with an additional type @m@ that allows to use monadic effects when checking for the goal or calculating the heuristic.
data SearchGoalM m c n = SearchGoalM {isGoalM :: n -> m Bool, getHeuristicM :: n -> m c}

-- | Check if a node match the `SearchGoal?
isGoal ::
  -- | A `SearchGoal?
  SearchGoal c n ->
  -- | A node
  n ->
  -- | `True` if the node matches the goal. Otherwise `False`
  Bool
isGoal (SearchGoalM f _) a = runIdentity $ f a

-- | Calculate the heuristic (estimated cost) between a node and the `SearchGoal`
getHeuristic ::
  -- | A `SearchGoal`
  SearchGoal c n ->
  -- | A node
  n ->
  -- The heuristic from the node the the goal
  c
getHeuristic (SearchGoalM _ g) a = runIdentity $ g a

instance (Ord c, Monad m) => Semigroup (SearchGoalM m c n) where
  (SearchGoalM f x) <> (SearchGoalM g y) =
    let h a = liftM2 (||) (f a) (g a)
        z a = liftM2 min (x a) (y a)
     in SearchGoalM h z

instance (Monoid c, Ord c, Monad m) => Monoid (SearchGoalM m c n) where
  mempty = SearchGoalM (const $ return False) (const $ return mempty)

-- | Create a `SearchGoal` from a goal-function and a heuristic-function
heuristic ::
  -- | For a given node return `True` if the node is the goal of the search
  (n -> Bool) ->
  -- | For a given node return the estimated cost to the goal
  (n -> c) ->
  -- | The resulting `SearchGoal`
  SearchGoal c n
heuristic f g = heuristicM (return . f) (return . g)

-- | Create a `SearchGoal` from a goal-function without a heuristic
simple ::
  (Monoid b) =>
  -- | For a given node return `True` if the node is the goal of the search
  (n -> Bool) ->
  -- | The resulting `SearchGoal`
  SearchGoal b n
simple f = simpleM (return . f)

-- | Like `heuristic` but with the possibility of using monadic effects during the computation
heuristicM :: (n -> m Bool) -> (n -> m c) -> SearchGoalM m c n
heuristicM = SearchGoalM

-- | Like `simple` but with the possibility of using monadic effects during the computation
simpleM :: (Monoid b, Monad m) => (n -> m Bool) -> SearchGoalM m b n
simpleM f = SearchGoalM f $ const (return mempty)