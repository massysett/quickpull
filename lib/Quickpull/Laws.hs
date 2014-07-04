-- | Functions to build 'TestTree' that test properties of
-- typeclasses, such as the functor laws, monad laws, and monoid laws.
-- These functions are rough; for example, they do not shrink on
-- failure, they are monomorphic, and they do not show the
-- counterexamples of failing functions.  But they are sufficient to
-- verify the lawfulness of your types.
module Quickpull.Laws where

import Quickpull.Types
import Test.QuickCheck

-- | Tests the monad laws:
--
-- Left identity: @return a >>= f@ equals @f a@
--
-- Right identity: @m >>= return@ equals @m@
--
-- Associativity: @(m >>= f) >>= g@ equals @m >>= (\x -> f x >>= g)@
monad
  :: (Eq b, Show b, Monad m)
  => Gen (m Int)
  -- ^ Generates a computation in the monad.

  -> Gen (Int -> m Int)
  -- ^ Generates a function that, when applied to an Int, returns a
  -- computation in the monad.

  -> Gen (m Int -> b)
  -- ^ Generates a function that runs a computation in the monad.

  -> TestTree

monad genK genF genU = group "monad laws"
  [ test "left identity" $ monadLeftIdentity genF genU
  , test "right identity" $ monadRightIdentity genK genU
  , test "associativity" $ monadAssociativity genK genF genU
  ]

monadLeftIdentity
  :: (Eq b, Show b, Monad m)
  => Gen (Int -> m Int)
  -> Gen (m Int -> b)
  -> Gen Property
monadLeftIdentity genK genUnwrap = do
  i <- arbitrary
  f <- genK
  u <- genUnwrap
  return $ (u (return i >>= f)) === u (f i)

monadRightIdentity
  :: (Eq b, Show b, Monad m)
  => Gen (m Int)
  -> Gen (m Int -> b)
  -> Gen Property
monadRightIdentity genM genUnwrap = do
  m <- genM
  u <- genUnwrap
  return $ (u (m >>= return)) === u m

monadAssociativity
  :: (Eq b, Show b, Monad m)
  => Gen (m Int)
  -> Gen (Int -> m Int)
  -> Gen (m Int -> b)
  -> Gen Property
monadAssociativity genM genK genUnwrap = do
  m <- genM
  f <- genK
  g <- genK
  u <- genUnwrap
  return $ (u ((m >>= f) >>= g)) === (u (m >>= (\x -> f x >>= g)))

-- | Tests the functor laws:
--
-- @fmap id@ equals @id@
--
-- @fmap (f . g)@ equals @fmap f . fmap g@

functor
  :: (Eq b, Show b, Functor f)
  => Gen (f Int)
  -- ^ Generates a computation in the functor.

  -> Gen (f Int -> b)
  -- ^ Generates a computation that unwraps the functor.

  -> TestTree
functor genK genU = group "functor laws"
  [ test "identity" tIdentity
  , test "composition" tComposition
  ]
  where
    tIdentity = do
      k <- genK
      u <- genU
      return $ (u (fmap id k)) === (u (id k))

    tComposition = do
      k <- genK
      u <- genU
      f <- arbitrary
      let _types = f :: Int -> Int
      g <- arbitrary
      return $ (u (fmap (f . g) k)) === (u ((fmap f . fmap g) k))
