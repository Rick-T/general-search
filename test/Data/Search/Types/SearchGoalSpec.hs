module Data.Search.Types.SearchGoalSpec where

import Control.Monad (liftM2)
import Data.Monoid (Sum)
import Data.Search.Types.SearchGoal
  ( SearchGoal,
    SearchGoalM (getHeuristicM, isGoalM),
    getHeuristic,
    heuristic,
    heuristicM,
    isGoal,
    simple,
    simpleM,
  )
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Testable (property))
import Test.QuickCheck.Function (apply)

spec :: Spec
spec = do
  describe "SearchGoal" $ do
    it "should combine targets with '||'" $
      property $
        \(funF, funG, x) ->
          let f = (apply funF :: Int -> Bool)
              g = (apply funG :: Int -> Bool)
           in isGoal (simple f <> simple g :: SearchGoal () Int) (x :: Int) == f x || g x
    it "should combine heuristics with 'min'" $
      property $
        \(funF, funG, x) ->
          let f = (apply funF :: Int -> Sum Float)
              g = (apply funG :: Int -> Sum Float)
           in getHeuristic (heuristic (const True) f <> heuristic (const True) g :: SearchGoal (Sum Float) Int) (x :: Int) == min (f x) (g x)
    it "should combine targets with '||' (monadic)" $
      property $
        \(funF, funG, x) ->
          let f = (apply funF :: Int -> Maybe Bool)
              g = (apply funG :: Int -> Maybe Bool)
           in isGoalM (simpleM f <> simpleM g :: SearchGoalM Maybe () Int) (x :: Int) == liftM2 (||) (f x) (g x)
    it "should combine heuristics with 'min' (monadic)" $
      property $
        \(funF, funG, x) ->
          let f = (apply funF :: Int -> Maybe (Sum Float))
              g = (apply funG :: Int -> Maybe (Sum Float))
           in getHeuristicM (heuristicM (return . const True) f <> heuristicM (return . const True) g :: SearchGoalM Maybe (Sum Float) Int) (x :: Int) == liftM2 min (f x) (g x)