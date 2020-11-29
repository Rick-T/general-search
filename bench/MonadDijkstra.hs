{-# LANGUAGE FlexibleContexts #-}

module MonadDijkstra where

import Control.Monad (msum)
import Control.Monad.Search (cost, runSearchBest)
import Data.Monoid (Sum (Sum))
import Data.Search.DFS.Hashable (fromSeparate, getEdges)
import MovingAi.Navigation
  ( diagonalDistance,
    euclidianDistance,
    searchStep,
  )
import MovingAi.Scenarios
  ( MovingAiMap,
    MovingAiScenario (MovingAiScenario),
  )

solve :: MovingAiScenario -> MovingAiMap -> Either String ()
solve (MovingAiScenario _ _ _ _ startX startY goalX goalY expected) map =
  let start = (startX, startY)
      to = (goalX, goalY)
      neighbours = getEdges $ fromSeparate (searchStep map) (\a b -> Just $ euclidianDistance a b)
      distance = diagonalDistance
      go ls =
        if head ls == to
          then return ls
          else msum $
            flip Prelude.map (neighbours (head ls)) $
              \(l, d) -> do
                cost d (distance l to)
                go (l : ls)
      result = runSearchBest $ go [start]
   in case result of
        Nothing -> Left "Found no solution"
        Just (Sum cost, _) -> if abs ((cost - expected) / expected) < 1e-3 then Right () else Left $ "Expected " ++ show expected ++ " but got " ++ show cost