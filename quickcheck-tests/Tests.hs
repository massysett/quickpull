-- | Example tests on the QuickCheck 'Gen' monad.
--
-- The tests 'proptree_quickcheckGenMonadLaws' and
-- 'proptree_quickcheckGenApplicativeLaws' will fail, because 'Gen'
-- does not obey the monad laws if you interpret them strictly, see:
--
-- <http://stackoverflow.com/questions/24481648/quickcheck-gen-is-not-a-monad>
--
-- This module is useful as a more intricate example of the use of
-- Quickpull, as well as an example of tests that fail (unlike the
-- tests included in the test suite, as these should always pass.)
module Tests where

import Quickpull
import Quickpull.Laws
import Test.QuickCheck
import Test.QuickCheck.Gen
import Prelude.Generators (function1)

genGen :: Gen (Gen Int)
genGen = do
  f <- function1 coarbitraryShow arbitrary
  return $ MkGen (curry f)

genFGen :: Gen (Int -> Gen Int)
genFGen = do
  i <- arbitrary :: Gen Int
  gi <- genGen
  return $ \j -> variant i . variant j $ gi

genRunner :: Gen (Gen Int -> Int)
genRunner = MkGen $ \g s ->
  \(MkGen f) -> f g s

genFunction :: Gen (Gen (Int -> Int))
genFunction = do
  i <- arbitrary :: Gen Int
  return $ variant i arbitrary

proptree_quickcheckGenMonadLaws :: TestTree
proptree_quickcheckGenMonadLaws = monad genGen genFGen genRunner

proptree_quickcheckGenFunctorLaws :: TestTree
proptree_quickcheckGenFunctorLaws = functor genGen genRunner

proptree_quickcheckGenApplicativeLaws :: TestTree
proptree_quickcheckGenApplicativeLaws =
  applicative genGen genFunction genRunner
