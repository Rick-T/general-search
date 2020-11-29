{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Search.DFS.Internal where

import Data.Search.Classes.OpenSet (OpenSet (..))

newtype Stack v c a = Stack {unwind :: [(v, c, a)]}

instance OpenSet (Stack v c a) v c a where
  insertWithPriority v c a (Stack l) = Stack ((v, c, a) : l)
  minView (Stack []) = Nothing
  minView (Stack ((v, c, a) : l)) = Just (v, c, a, Stack l)
  singleton v c a = Stack [(v, c, a)]
  empty = Stack []
