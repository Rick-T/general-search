module DFS where

import Data.Monoid (Sum (..))
import Data.Search.DFS.Hashable
  ( depthFirst,
    fromSeparate,
    heuristic,
    noAccumulator,
    runSearch,
    search,
  )
import MovingAi.Navigation (edgeCost, searchStep)
import MovingAi.Scenarios
  ( MovingAiMap,
    MovingAiScenario (MovingAiScenario),
  )

solve :: MovingAiScenario -> MovingAiMap -> Either String ()
solve (MovingAiScenario _ _ _ _ startX startY goalX goalY expected) map =
  let searchGoal = heuristic (== (goalX, goalY)) (const $ Sum 1)
      graph = fromSeparate (searchStep map) (edgeCost map)
      result = runSearch (search searchGoal) depthFirst noAccumulator graph (startX, startY)
   in case result of
        Nothing -> Left "Found no solution"
        (Just (_, Sum cost, _)) -> if abs ((cost - expected) / expected) < 1e-3 then Right () else Left $ "Expected " ++ show expected ++ " but got " ++ show cost
