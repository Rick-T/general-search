module GeneralM where

import Control.Monad.Identity (Identity (..))
import Data.Monoid (Sum (..))
import Data.Search.BFS.Hashable
  ( breadthFirst,
    fromSeparateM,
    heuristicM,
    noAccumulator,
    runSearchT,
    searchM,
  )
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
  let searchGoal = heuristicM (return . (== (goalX, goalY))) (return . (diagonalDistance (goalX, goalY)))
      graph = fromSeparateM (\a -> return $ searchStep map a) (\a b -> return $ edgeCost map a b)
      result = runSearchT (searchM searchGoal) breadthFirst noAccumulator graph (startX, startY)
   in case result of
        Identity Nothing -> Left "Found no solution"
        Identity (Just (_, Sum cost, _)) -> if abs ((cost - expected) / expected) < 1e-3 then Right () else Left $ "Expected " ++ show expected ++ " but got " ++ show cost