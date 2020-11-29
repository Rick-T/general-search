{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Search.ExampleGraph where

-- Use example from german Wikipedia because it's cute
-- https://de.wikipedia.org/wiki/A*-Algorithmus#/media/Datei:Astar-germany0.svg

import Data.Hashable (Hashable)
import Data.Monoid (Sum)
import Data.Search.Types
  ( Graph,
    SearchGoal,
    fromCombined,
    heuristic,
    simple,
  )
import GHC.Generics (Generic)

data City = SB | KA | HN | WÜ | KL | LU | F deriving (Generic, Eq, Ord, Show, Enum, Bounded, Hashable)

directedEdges :: City -> [(City, Sum Int)]
directedEdges SB = [(KA, return 145), (KL, return 70)]
directedEdges KA = [(HN, return 84)]
directedEdges HN = [(WÜ, return 102)]
directedEdges WÜ = []
directedEdges LU = [(WÜ, return 183)]
directedEdges KL = [(LU, return 53), (F, return 103)]
directedEdges F = [(WÜ, return 116)]

undirectedEdges :: City -> [(City, Sum Int)]
undirectedEdges SB = [(KA, return 145), (KL, return 70)]
undirectedEdges KA = [(SB, return 145), (HN, return 84)]
undirectedEdges HN = [(KA, return 84), (WÜ, return 102)]
undirectedEdges WÜ = [(HN, return 102), (LU, return 183), (F, return 96)]
undirectedEdges LU = [(KL, return 158), (WÜ, return 183)]
undirectedEdges KL = [(SB, return 70), (LU, return 53), (F, return 103)]
undirectedEdges F = [(KL, return 103), (WÜ, return 116)]

heur :: City -> Sum Int
heur SB = 222
heur KA = 140
heur HN = 87
heur WÜ = 0
heur KL = 158
heur LU = 108
heur F = 96

directedGraph :: Graph [] (Sum Int) City
directedGraph = fromCombined directedEdges

undirectedGraph :: Graph [] (Sum Int) City
undirectedGraph = fromCombined undirectedEdges

würzburg :: SearchGoal (Sum Int) City
würzburg = heuristic (== WÜ) heur

ludwigshafen :: SearchGoal (Sum Int) City
ludwigshafen = simple (== LU)