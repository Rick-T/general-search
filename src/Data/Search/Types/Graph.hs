{-# LANGUAGE TupleSections #-}

module Data.Search.Types.Graph where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Foldable (Foldable (toList))
import Data.Maybe (catMaybes, mapMaybe)

-- | A `Graph` is a type-wrapper around functions of type @a -> f(a, c)@, mapping nodes of the graph to their neighbours with the associated costs.
--
-- @a@ is the type of the nodes of the graph
--
-- @c@ is the type of costs for moving between two nodes
--
-- @f@ is a `Foldable`
type Graph f c v = GraphM Identity f c v

-- | Like `Graph` but instead wrapping functions of type @a -> m (f(a, c))@, allowing the use of monadic effects when evalutating graph edges
newtype GraphM m f c v = GraphM (v -> m (f (v, c)))

-- | For a given node get all neighbours with their associated costs from a `Graph`
getEdges ::
  -- | A graph
  Graph f c v ->
  -- | A node
  v ->
  -- | The neighbours of the node together with the costs of moving there
  f (v, c)
getEdges (GraphM edges) a = runIdentity $ edges a
{-# INLINEABLE getEdges #-}

-- | Create a `Graph` from a neighbours-function and a cost-function
fromSeparate ::
  (Foldable f) =>
  -- | A function returning all the neighbours for a given node
  (v -> f v) ->
  -- | A function returning the costs for moving between a pair of nodes. May return `Nothing` if there is no edge between them
  (v -> v -> Maybe c) ->
  -- | The resulting graph
  Graph [] c v
fromSeparate edges cost = GraphM $ \a ->
  return $
    mapMaybe
      (\b -> (b,) <$> cost a b)
      $ toList $ edges a
{-# INLINEABLE fromSeparate #-}

-- | Create a `Graph` from a single function that return all neighbours of a node together with the cost of moving to them
fromCombined :: (v -> f (v, c)) -> Graph f c v
fromCombined f = GraphM $ \a -> return $ f a
{-# INLINEABLE fromCombined #-}

-- | Like `getEdges` for a `GraphM`
getEdgesM :: GraphM m f c v -> v -> m (f (v, c))
getEdgesM (GraphM edges) = edges
{-# INLINEABLE getEdgesM #-}

-- | Like `fromSeparate` for functions that can use monadic effects
fromSeparateM :: (Monad m, Foldable f) => (v -> m (f v)) -> (v -> v -> m (Maybe c)) -> GraphM m [] c v
fromSeparateM edgesM costM = GraphM $ \a -> do
  edges <- edgesM a
  neighbours <-
    mapM
      ( \b -> do
          cost <- costM a b
          return $ (b,) <$> cost
      )
      $ toList edges
  return $ catMaybes neighbours
{-# INLINE fromSeparateM #-}

-- | Like `fromCombined` for functions that can use monadic effects
fromCombinedM :: (v -> m (f (v, c))) -> GraphM m f c v
fromCombinedM = GraphM
{-# INLINEABLE fromCombinedM #-}
