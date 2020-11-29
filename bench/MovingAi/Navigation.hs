module MovingAi.Navigation where

import Control.DeepSeq (NFData)
import Data.Array (Array, accumArray, (!))
import Data.Monoid (Sum (Sum))
import GHC.Generics (Generic)
import MovingAi.Scenarios
import Paths_general_search (getDataFileName)

searchStep :: MovingAiMap -> (Int, Int) -> [(Int, Int)]
searchStep map pos = [neighbour | neighbour <- moveOct pos, canMove map pos neighbour]

moveOct :: (Int, Int) -> [(Int, Int)]
moveOct (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

edgeCost :: MovingAiMap -> (Int, Int) -> (Int, Int) -> Maybe (Sum Float)
edgeCost map p q =
  if canMove map p q then Just $ euclidianDistance p q else Nothing

euclidianDistance :: (Int, Int) -> (Int, Int) -> Sum Float
euclidianDistance (x, y) (x', y') = Sum $ sqrt ((fromIntegral (x - x') ^ 2) + (fromIntegral (y - y') ^ 2))

diagonalDistance :: (Int, Int) -> (Int, Int) -> Sum Float
diagonalDistance (x, y) (x', y') =
  let dx = fromIntegral $ abs (x - x')
      dy = fromIntegral $ abs (y - y')
      sqrt2 = sqrt 2
   in Sum $ (dx + dy) + (sqrt2 - 2) * min dx dy

canMove :: MovingAiMap -> (Int, Int) -> (Int, Int) -> Bool
canMove map p@(px, py) q@(qx, qy)
  | px == qx || py == qy = canEnterTile from to
  | otherwise = canEnterTile from to && canMove map p corner1 && canMove map p corner2
  where
    from = map ! p
    to = map ! q
    corner1 = (px, qy)
    corner2 = (qx, py)

canEnterTile :: Terrain -> Terrain -> Bool
canEnterTile Unpassable _ = error "How did I end up here?"
canEnterTile _ Passable = True
canEnterTile _ Unpassable = False
canEnterTile from Water = from == Water
canEnterTile from Swamp = from == Swamp || from == Passable
