{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Search.Pure.Types.Search where

import Control.Monad.State
  ( MonadState (get),
    State,
    StateT (StateT),
    evalState,
    modify,
  )
import Data.Search.Classes.ClosedSet as CS (ClosedSet (..))
import Data.Search.Classes.OpenSet as OS (OpenSet (..))
import Data.Search.Pure.Types.Graph (GraphP, getEdgesP)
import Data.Search.Pure.Types.SearchGoal
  ( SearchGoalP (getHeuristicP, isGoalP),
  )
import Data.Search.Types
  ( Accumulator (getAccumulatorFunction, getEmptyAccumulator),
    HeuristicCost (HeuristicCost, getRealCost),
    SearchAlgorithm,
  )
import Data.Search.Types.Search.Internal (StepResult (..))

newtype SearchP a b c f q h r = Search {getSearch :: State (GeneralSearchStateP a b c f q h) r} deriving (Functor, Applicative, Monad, MonadState (GeneralSearchStateP a b c f q h))

data GeneralSearchStateP a b c f q h = GeneralSearchStateP
  { openSetState :: q,
    closedSetState :: h,
    graphState :: GraphP f c a,
    accumulatorFunctionState :: a -> b -> b
  }

searchP :: (OpenSet q a (HeuristicCost c) b, ClosedSet h a, Foldable f, Semigroup c) => SearchGoalP c a -> SearchP a b c f q h (Maybe (a, c, b))
searchP target = do
  result <- searchStepP target
  case result of
    EXPANDED_NODE _ -> searchP target
    EXPANDED_GOAL ret -> return $ Just ret
    EXHAUSTED -> return Nothing
{-# INLINEABLE searchP #-}

searchStepP :: (MonadState (GeneralSearchStateP a b c f q h) m, ClosedSet h a, OpenSet q a (HeuristicCost c) b, Foldable f, Semigroup c) => SearchGoalP c a -> m (StepResult a c b)
searchStepP target = do
  (GeneralSearchStateP q v graph append) <- get
  case minView q of
    Nothing -> return EXHAUSTED
    Just (a, value, acc, q') -> do
      let acc' = append a acc
      let v' = insert a v
      let q'' = foldr (\(neighbour, realCost) queue -> if neighbour `member` v' then queue else insertWithPriority neighbour (value <> HeuristicCost realCost (getHeuristicP target neighbour)) acc' queue) q' $ getEdgesP graph a
      modify $ \s -> s {openSetState = q'', closedSetState = v'}
      if isGoalP target a then return $ EXPANDED_GOAL (a, getRealCost value, acc') else return $ EXPANDED_NODE (a, getRealCost value, acc')
{-# INLINE searchStepP #-}

searchAllP :: (Semigroup c, OpenSet q a (HeuristicCost c) b, ClosedSet h a, Foldable f) => SearchGoalP c a -> SearchP a b c f q h [(a, c, b)]
searchAllP goal =
  searchP goal >>= \mResult -> case mResult of
    Nothing -> return []
    Just result -> do
      next <- searchAllP goal
      return $ result : next
{-# INLINE searchAllP #-}

runSearchP :: (OpenSet q a (HeuristicCost c) b, ClosedSet h a, Monoid c) => SearchP a b c f q h r -> SearchAlgorithm q h -> Accumulator b a -> GraphP f c a -> a -> r
runSearchP srch _ acc graph start =
  let closedSet = CS.empty
      openSet = singleton start mempty $ getEmptyAccumulator acc
   in evalState (getSearch srch) $ GeneralSearchStateP openSet closedSet graph $ getAccumulatorFunction acc
{-# INLINE runSearchP #-}