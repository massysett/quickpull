module Tests where

import Quickpull
import Quickpull.Function
import Quickpull.Laws
import Test.QuickCheck
import Test.QuickCheck.Gen

genGen :: Gen (Gen Int)
genGen = do
  f <- function coarbitraryShow arbitrary
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

-- | Verifies that the QuickCheck 'Gen' monad does (not) satisfy the
-- monad laws.
proptree_quickcheckGenMonadLaws :: TestTree
proptree_quickcheckGenMonadLaws = monad genGen genFGen genRunner

proptree_quickcheckGenFunctorLaws :: TestTree
proptree_quickcheckGenFunctorLaws = functor genGen genRunner

proptree_quickcheckGenApplicativeLaws :: TestTree
proptree_quickcheckGenApplicativeLaws =
  applicative genGen genFunction genRunner
