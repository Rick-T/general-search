module SearchAlgorithms where

import Algorithm.Search (aStar)
import Data.Monoid (Sum (Sum))
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
  let goal = (goalX, goalY)
      edges a = searchStep map a
      result = aStar edges euclidianDistance (diagonalDistance goal) (== goal) (startX, startY)
   in case result of
        Nothing -> Left "Found no solution"
        Just (Sum cost, _) -> if abs ((cost - expected) / expected) < 1e-3 then Right () else Left $ "Expected " ++ show expected ++ " but got " ++ show cost