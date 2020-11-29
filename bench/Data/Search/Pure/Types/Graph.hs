{-# LANGUAGE TupleSections #-}

module Data.Search.Pure.Types.Graph where

import Data.Foldable (Foldable (toList))
import Data.Maybe (mapMaybe)

newtype GraphP f c a = GraphP (a -> f (a, c))

getEdgesP :: GraphP f c a -> a -> f (a, c)
getEdgesP (GraphP edges) = edges
{-# INLINEABLE getEdgesP #-}

fromCombinedP :: (a -> f (a, c)) -> GraphP f c a
fromCombinedP = GraphP
{-# INLINEABLE fromCombinedP #-}

fromSeparateP :: (Foldable f) => (a -> f a) -> (a -> a -> Maybe c) -> GraphP [] c a
fromSeparateP edges cost = GraphP $ \a ->
  mapMaybe
    (\b -> (b,) <$> cost a b)
    $ toList $ edges a
{-# INLINEABLE fromSeparateP #-}