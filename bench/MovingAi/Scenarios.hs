{-# LANGUAGE DeriveGeneric #-}

module MovingAi.Scenarios where

import Control.DeepSeq (NFData)
import Data.Array (Array, accumArray)
import GHC.Generics (Generic)
import Paths_general_search (getDataFileName)

-- * Data types

data Terrain = Passable | Unpassable | Water | Swamp deriving (Generic, Show, Eq, Enum)

type MovingAiMap = Array (Int, Int) Terrain

data MovingAiScenario = MovingAiScenario {getBucket :: Int, getMap :: String, getMapWidth :: Int, getMapHeight :: Int, getStartX :: Int, getStartY :: Int, getGoalX :: Int, getGoalY :: Int, getOptimalLength :: Float} deriving (Generic, Show)

instance NFData MovingAiScenario

instance NFData Terrain

data MovingAiResource = MovingAiResource {getResourceType :: String, getResourceName :: String}

-- * Loading Moving AI resources

loadResource :: MovingAiResource -> IO ([MovingAiScenario], MovingAiMap)
loadResource resource = do
  scenarios <- readScenarios <$> (readFile =<< resourcePath (getScenarioName resource))
  map <- readMap <$> (readFile =<< resourcePath (getMapName resource))
  return (scenarios, map)

readMap :: String -> MovingAiMap
readMap map =
  let [_, [_, height], [_, width]] = take 3 $ words <$> lines map
   in accumArray (\_ a -> a) Unpassable ((-1, -1), (read width, read height)) [((x, y), parseTerrain tile) | (y, line) <- zip [0 ..] (drop 4 $ lines map), (x, tile) <- zip [0 ..] line]

parseTerrain :: Char -> Terrain
parseTerrain '.' = Passable
parseTerrain 'G' = Passable
parseTerrain '@' = Unpassable
parseTerrain 'O' = Unpassable
parseTerrain 'T' = Unpassable
parseTerrain 'S' = Swamp
parseTerrain 'W' = Water
parseTerrain c = error $ "The docs lied :( Found invalid character " ++ [c]

resourcePath :: String -> IO String
resourcePath name = getDataFileName $ movingAiResourcePrefix ++ name

readScenarios :: String -> [MovingAiScenario]
readScenarios = fmap readScenario . tail . lines

readScenario :: String -> MovingAiScenario
readScenario scen =
  let [bucket, mapName, mapWidth, mapHeight, startX, startY, goalX, goalY, optimalLength] = words scen
   in MovingAiScenario (read bucket) mapName (read mapWidth) (read mapHeight) (read startX) (read startY) (read goalX) (read goalY) (read optimalLength)

getScenarioName :: MovingAiResource -> String
getScenarioName resource = getMapName resource ++ ".scen"

getMapName :: MovingAiResource -> String
getMapName (MovingAiResource resourceType resourceName) = resourceType ++ "/" ++ resourceName ++ ".map"

-- * Constants

movingAiResourcePrefix :: String
movingAiResourcePrefix = "bench/resources/movingai/"

expedition :: MovingAiResource
expedition = MovingAiResource "starcraft" "Expedition"

floodedPlains :: MovingAiResource
floodedPlains = MovingAiResource "starcraft" "FloodedPlains"

gnollwood :: MovingAiResource
gnollwood = MovingAiResource "warcraft_3" "gnollwood"

lak303d :: MovingAiResource
lak303d = MovingAiResource "dragon_age_origins" "lak303d"

losttemple :: MovingAiResource
losttemple = MovingAiResource "warcraft_3" "losttemple"

maze512_1_1 :: MovingAiResource
maze512_1_1 = MovingAiResource "maze" "maze512-1-1"

maze512_8_1 :: MovingAiResource
maze512_8_1 = MovingAiResource "maze" "maze512-8-1"

maze512_32_1 :: MovingAiResource
maze512_32_1 = MovingAiResource "maze" "maze512-32-1"

random512_10_1 :: MovingAiResource
random512_10_1 = MovingAiResource "random" "random512-10-1"

random512_25_1 :: MovingAiResource
random512_25_1 = MovingAiResource "random" "random512-25-1"

random512_40_1 :: MovingAiResource
random512_40_1 = MovingAiResource "random" "random512-40-1"

room8_1 :: MovingAiResource
room8_1 = MovingAiResource "room" "8room_001"

room16_1 :: MovingAiResource
room16_1 = MovingAiResource "room" "16room_001"

room64_1 :: MovingAiResource
room64_1 = MovingAiResource "room" "64room_001"
