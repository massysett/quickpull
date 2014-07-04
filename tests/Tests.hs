module Tests where

import Quickpull
import Quickpull.Laws
import Test.QuickCheck

proptree_maybeMonadLaws :: TestTree
proptree_maybeMonadLaws = monad arbitrary arbitrary (return same)
  where
    same :: Maybe Int -> Maybe Int
    same = id