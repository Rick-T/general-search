{-# LANGUAGE FlexibleContexts #-}

import qualified Astar
import qualified AstarM
import Criterion.Main
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    env,
    nf,
  )
import qualified DFS
import qualified General
import qualified GeneralM
import qualified GeneralOrd
import qualified GeneralOrdM
import qualified MonadDijkstra
import MovingAi.Scenarios
  ( MovingAiMap,
    MovingAiResource (getResourceName, getResourceType),
    MovingAiScenario,
    expedition,
    floodedPlains,
    gnollwood,
    lak303d,
    loadResource,
    losttemple,
    maze512_1_1,
    maze512_32_1,
    maze512_8_1,
    random512_10_1,
    random512_25_1,
    random512_40_1,
    room16_1,
    room64_1,
    room8_1,
  )
import qualified Reference
import qualified SearchAlgorithms
import qualified SearchAlgorithmsM

main :: IO ()
main = do
  defaultMain
    [ scenarioBenchIO losttemple,
      scenarioBenchIO gnollwood,
      scenarioBenchIO floodedPlains,
      scenarioBenchIO expedition,
      scenarioBenchIO lak303d,
      scenarioBenchIO maze512_1_1,
      scenarioBenchIO maze512_8_1,
      scenarioBenchIO maze512_32_1,
      scenarioBenchIO random512_10_1,
      scenarioBenchIO random512_25_1,
      scenarioBenchIO random512_40_1,
      scenarioBenchIO room8_1,
      scenarioBenchIO room16_1,
      scenarioBenchIO room64_1
    ]

loadLast :: MovingAiResource -> IO (MovingAiScenario, MovingAiMap)
loadLast resource = do
  (scenarios, map) <- loadResource resource
  return (last scenarios, map)

scenarioBenchIO :: MovingAiResource -> Benchmark
scenarioBenchIO resource = env (loadLast resource) $ \ ~(scenario, map) ->
  bgroup
    (getResourceType resource ++ "/" ++ getResourceName resource)
    [ bench "astar" $ nf (`Astar.solve` map) scenario,
      bench "astar (monadic)" $ nf (`AstarM.solve` map) scenario,
      --bench "dfs (hashable)" $ nf (`DFS.solve` map) scenario,
      bench "general-search (hashable)" $ nf (`General.solve` map) scenario,
      bench "general-search (hashable, monadic)" $ nf (`GeneralM.solve` map) scenario,
      bench "general-search (ord)" $ nf (`GeneralOrd.solve` map) scenario,
      bench "general-search (ord, monadic)" $ nf (`GeneralOrdM.solve` map) scenario,
      --bench "monad-dijkstra" $ nf (`MonadDijsktra.solve` map) scenario,
      bench "reference" $ nf (`Reference.solve` map) scenario,
      bench "search-algorithms" $ nf (`SearchAlgorithms.solve` map) scenario,
      bench "search-algorithms (monadic)" $ nf (`SearchAlgorithmsM.solve` map) scenario
    ]
