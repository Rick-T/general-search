module SearchAlgorithmsM where

import Algorithm.Search (aStarM)
import Control.Monad.Identity (Identity (Identity))
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
      edges a = return $ searchStep map a
      result = aStarM edges (\a b -> return $ euclidianDistance a b) (return . diagonalDistance goal) (return . (== goal)) (startX, startY)
   in case result of
        Identity Nothing -> Left "Found no solution"
        Identity (Just (Sum cost, _)) -> if abs ((cost - expected) / expected) < 1e-3 then Right () else Left $ "Expected " ++ show expected ++ " but got " ++ show cost