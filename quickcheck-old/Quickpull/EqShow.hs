-- | This module is necessary to maintain compatibility with both
-- QuickCheck 2.6 and QuickCheck 2.7.  This module is used for older
-- versions of QuickCheck (before version 2.7).
module Quickpull.EqShow ((===)) where

import Test.QuickCheck

infix 4 ===

-- | Like '==', but prints a counterexample when it fails.
(===) :: (Eq a, Show a) => a -> a -> Property
x === y =
  printTestCase (show x ++ " /= " ++ show y) (x == y)
