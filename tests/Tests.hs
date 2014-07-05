module Tests where

import Quickpull
import Quickpull.Laws
import Test.QuickCheck

maybefy :: Maybe Int -> Maybe Int
maybefy = id

proptree_maybeMonadLaws :: TestTree
proptree_maybeMonadLaws = monad arbitrary arbitrary (return maybefy)

proptree_maybeFunctorLaws :: TestTree
proptree_maybeFunctorLaws = functor arbitrary (return maybefy)

proptree_maybeApplicativeLaws :: TestTree
proptree_maybeApplicativeLaws =
  applicative arbitrary arbitrary (return maybefy)

proptree_listMonoidLaws :: TestTree
proptree_listMonoidLaws =
  monoid (listOf arbitrary) (return (id :: [Int] -> [Int]))

prop_addIsAssociative :: Gen Property
prop_addIsAssociative =
  associative (return (+)) (return (id :: Int -> Int)) arbitrary

prop_addIsCommutative :: Gen Property
prop_addIsCommutative =
  commutative (return (+)) (return (id :: Int -> Int)) arbitrary

prop_zeroAddLeftIdentity :: Gen Property
prop_zeroAddLeftIdentity =
  leftIdentity (return (+)) (return (id :: Int -> Int))
               (return 0) arbitrary

prop_oneDivideRightIdentity :: Gen Property
prop_oneDivideRightIdentity =
  rightIdentity (return div) (return (id :: Int -> Int))
                (return 1) arbitrary
