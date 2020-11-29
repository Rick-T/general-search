-- | An `Accumulator` provides a way of combining multiple values of one type into a single element of (possibly) another type
module Data.Search.Types.Accumulator where

import Data.Set (Set, insert)

-- | An @Accumulator a v@ containes functions to combine values of type @v@ with a value of type @a@
data Accumulator a v = Accumulator
  { -- | Provide an empty value of type @a@
    getEmptyAccumulator :: a,
    -- | Combine another element of type @v@ with the value of type @a@
    getAccumulatorFunction :: v -> a -> a
  }

-- | `Accumulator` that does nothing
noAccumulator :: Accumulator () v
noAccumulator = Accumulator () $ const . const $ ()

-- | `Accumulator` that combines elements into a `List`
listAccumulator :: Accumulator [v] v
listAccumulator = Accumulator [] (:)

-- | `Accumulator` for elements implementing `Monoid` that adds them together
monoidAccumulator :: Monoid v => Accumulator v v
monoidAccumulator = Accumulator mempty (<>)

-- | `Accumulator` that combines elements into a `Set`
setAccumulator :: (Ord v) => Accumulator (Set v) v
setAccumulator = Accumulator mempty insert

-- | Turn an `Accumulator` that contains @v@s into an `Accumulator` that contains @w@s
contramap :: (v -> w) -> Accumulator a w -> Accumulator a v
contramap f (Accumulator empty func) = Accumulator empty $ func . f

-- | Turn an `Accumulator` that contains @v@s into an `Accumulator` that contains @w@s, discarding elements for which the given function is `Nothing`
contramapMaybe :: (v -> Maybe w) -> Accumulator a w -> Accumulator a v
contramapMaybe f (Accumulator empty func) = Accumulator empty $ \a b -> case f a of
  Nothing -> b
  (Just c) -> func c b

-- | Let the `Accumulator` only collect values that satisfy the given predicate
filter :: (v -> Bool) -> Accumulator a v -> Accumulator a v
filter f (Accumulator empty func) = Accumulator empty (\a b -> if f a then func a b else b)
