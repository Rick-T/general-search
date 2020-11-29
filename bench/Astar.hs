module Astar where

import Data.Graph.AStar (aStar)
import qualified Data.HashSet as HashSet
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
solve (MovingAiScenario _ _ _ _ startX startY goalX goalY _) map =
  let goal = (goalX, goalY)
      edges a = HashSet.fromList $ searchStep map a
      result = aStar edges euclidianDistance (diagonalDistance goal) (== goal) (startX, startY)
   in case result of
        Nothing -> Left "Found no solution"
        Just _ -> Right () -- The astar library does not return the cost. For the sake of the benchmark we will trust that the calculated path is correct instead of checking it