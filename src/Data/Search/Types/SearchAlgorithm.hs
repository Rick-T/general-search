module Data.Search.Types.SearchAlgorithm where

-- | A search algorithm is defined by the choice of `Data.Search.Classes.OpenSet.OpenSet` and `Data.Search.Classes.ClosedSet.ClosedSet`
data SearchAlgorithm q h = SearchAlgorithm
  { getOpenSet :: q,
    getClosedSet :: h
  }