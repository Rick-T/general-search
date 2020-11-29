module Data.Search.Pure.Types.SearchGoal where

data SearchGoalP c a = SearchGoalP {isGoalP :: a -> Bool, getHeuristicP :: a -> c}

instance (Ord c) => Semigroup (SearchGoalP c a) where
  (SearchGoalP f x) <> (SearchGoalP g y) =
    let h a = f a || g a
        z a = min (x a) (y a)
     in SearchGoalP h z

instance (Monoid c, Ord c) => Monoid (SearchGoalP c a) where
  mempty = SearchGoalP (const False) (const mempty)

heuristicP :: (a -> Bool) -> (a -> c) -> SearchGoalP c a
heuristicP = SearchGoalP

simpleP :: Monoid b => (a -> Bool) -> SearchGoalP b a
simpleP f = SearchGoalP f $ const mempty