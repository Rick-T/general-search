module Data.Search.Pure.GraphManip (connectAllP, connectAllP', filterEdgesP, filterEdgesP', modifyEdgesP) where

import Data.Foldable (Foldable (toList))
import Data.Hashable (Hashable)
import Data.Search.BFS.Hashable
  ( Accumulator,
    noAccumulator,
  )
import Data.Search.GraphManip.Internal
  ( dropAccumulator,
  )
import Data.Search.Pure.GraphManip.Internal
  ( modifyEdgesP,
    reduceGraphP,
  )
import Data.Search.Pure.Types.Graph (GraphP)

connectAllP :: (Foldable t, Foldable f, Hashable a, Ord a, Ord c, Monoid c) => t a -> (a -> Bool) -> GraphP f c a -> GraphP [] c a
connectAllP domain goal graph = modifyEdgesP dropAccumulator $ connectAllP' domain goal noAccumulator graph
{-# INLINEABLE connectAllP #-}

connectAllP' :: (Foldable t, Foldable f, Hashable a, Ord a, Ord c, Monoid c) => t a -> (a -> Bool) -> Accumulator b a -> GraphP f c a -> GraphP [] (c, b) a
connectAllP' = reduceGraphP $ const id
{-# INLINEABLE connectAllP' #-}

filterEdgesP :: (Foldable t, Foldable f, Hashable a, Ord a, Ord c, Monoid c) => t a -> (a -> Bool) -> GraphP f c a -> GraphP [] c a
filterEdgesP domain goal graph = modifyEdgesP dropAccumulator $ filterEdgesP' domain goal noAccumulator graph
{-# INLINEABLE filterEdgesP #-}

filterEdgesP' :: (Foldable t, Foldable f, Hashable a, Ord a, Ord c, Monoid c) => t a -> (a -> Bool) -> Accumulator b a -> GraphP f c a -> GraphP [] (c, b) a
filterEdgesP' domain goal = reduceGraphP (\start edges a -> if a /= start && goal a then [] else toList $ edges a) domain goal
{-# INLINEABLE filterEdgesP' #-}