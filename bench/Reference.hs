module Reference where

import Data.Monoid (Sum (..))
import Data.Search.BFS.Hashable (breadthFirst, noAccumulator)
import Data.Search.Pure.Types.Graph (fromSeparateP)
import Data.Search.Pure.Types.Search (runSearchP, searchP)
import Data.Search.Pure.Types.SearchGoal (heuristicP)
import MovingAi.Navigation
  ( diagonalDistance,
    edgeCost,
    searchStep,
  )
import MovingAi.Scenarios
  ( MovingAiMap,
    MovingAiScenario (MovingAiScenario),
  )

solve :: MovingAiScenario -> MovingAiMap -> Either String ()
solve (MovingAiScenario _ _ _ _ startX startY goalX goalY expected) map =
  let searchGoal = heuristicP (== (goalX, goalY)) (diagonalDistance (goalX, goalY))
      graph = fromSeparateP (searchStep map) (edgeCost map)
      result = runSearchP (searchP searchGoal) breadthFirst noAccumulator graph (startX, startY)
   in case result of
        Nothing -> Left "Found no solution"
        Just (_, Sum cost, _) -> if abs ((cost - expected) / expected) < 1e-3 then Right () else Left $ "Expected " ++ show expected ++ " but got " ++ show cost
