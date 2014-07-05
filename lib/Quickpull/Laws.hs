-- | Functions to build 'TestTree' that test properties of
-- typeclasses, such as the functor laws, monad laws, and monoid laws.
-- These functions are rough; for example, they do not shrink on
-- failure, they are monomorphic, and they do not show the
-- counterexamples of failing functions.  But they are sufficient to
-- help verify the lawfulness of your types.
module Quickpull.Laws where

import Control.Applicative
import Data.Monoid
import Quickpull.Types
import Test.QuickCheck

-- | Tests the monad laws:
--
-- Left identity:
--
-- > return a >>= f == f a
--
-- Right identity:
--
-- > m >>= return == m
--
-- Associativity:
--
-- > (m >>= f) >>= g == m >>= (\x -> f x >>= g)

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
  [ test "left identity" monadLeftIdentity
  , test "right identity" monadRightIdentity
  , test "associativity" monadAssociativity
  ]
  where

    monadLeftIdentity = do
      i <- arbitrary
      f <- genF
      u <- genU
      return $ (u (return i >>= f)) === u (f i)

    monadRightIdentity = do
      m <- genK
      u <- genU
      return $ (u (m >>= return)) === u m

    monadAssociativity = do
      m <- genK
      f <- genF
      g <- genF
      u <- genU
      return $ (u ((m >>= f) >>= g)) === (u (m >>= (\x -> f x >>= g)))

-- | Tests the functor laws:
--
-- > fmap id == id
--
-- > fmap (f . g) == fmap f . fmap g

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

-- | Tests the Applicative laws:
--
-- * identity:
--
-- > pure id <*> v == v
--
-- * composition:
--
-- > pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
--
-- * homomorphism:
--
-- > pure f <*> pure x = pure (f x)
--
-- * interchange:
--
-- > u <*> pure y = pure ($ y) <*> u


applicative
  :: (Eq b, Show b, Applicative f)
  => Gen (f Int)
  -- ^ Generates a computation in the Applicative.
  -> Gen (f (Int -> Int))
  -- ^ Generates a function in the Applicative.
  -> Gen (f Int -> b)
  -- ^ Generates an unwrapping function.
  -> TestTree
applicative gK gF gU = group "applicative laws"
  [ test "identity" tIdentity
  , test "composition" tComposition
  , test "homomorphism" tHomomorphism
  , test "interchange" tInterchange
  ]
  where
    tIdentity = do
      u <- gU
      v <- gK
      return $ (u (pure id <*> v)) === (u v)

    tComposition = do
      u <- gF
      v <- gF
      w <- gK
      r <- gU
      return $ (r (pure (.) <*> u <*> v <*> w)) ===
        (r (u <*> (v <*> w)))

    tHomomorphism = do
      f <- arbitrary
      let _types = f :: Int -> Int
      x <- arbitrary
      u <- gU
      return $ (u (pure f <*> pure x)) ===
        (u (pure (f x)))

    tInterchange = do
      r <- gU
      u <- gF
      y <- arbitrary
      return $ (r (u <*> pure y)) ===
        (r (pure ($ y) <*> u))

-- | Tests the monoid laws:
--
-- > mappend mempty x = x
--
-- > mappend x mempty = x
--
-- > mappend x (mappend y z) = mappend (mappend x y) z
--
-- > mconcat = foldr mappend mempty
monoid
  :: (Eq b, Show b, Monoid a)
  => Gen a
  -- ^ Generates monoid values
  -> Gen (a -> b)
  -- ^ Generates unwrappers
  -> TestTree
monoid gV gU = group "monoid laws"
  [ test "left identity" tLeft
  , test "right identity" tRight
  , test "associativity" tAssociative
  , test "mconcat = foldr" tFoldr
  ]
  where

    tLeft = do
      x <- gV
      u <- gU
      return $ (u (mappend mempty x)) === (u x)

    tRight = do
      x <- gV
      u <- gU
      return $ (u (mappend x mempty)) === (u x)

    tAssociative = do
      x <- gV
      y <- gV
      z <- gV
      u <- gU
      return $ (u (mappend x (mappend y z))) ===
        (u (mappend (mappend x y) z))

    tFoldr = do
      ls <- listOf gV
      u <- gU
      return $ (u (mconcat ls)) ===
        (u (foldr mappend mempty ls))

-- | Tests whether a particular operation is associative, that is:
--
-- > a `f` (b `f` c) == (a `f` b) `f` c
associative
  :: (Eq b, Show b)
  => Gen (a -> a -> a)
  -- ^ Generates an associative operation
  -> Gen (a -> b)
  -- ^ Generates unwrappers
  -> Gen a
  -- ^ Generates values
  -> Gen Property
associative gF gU gV = do
  f <- gF
  u <- gU
  a <- gV
  b <- gV
  c <- gV
  return $ (u (a `f` (b `f` c))) ===
    (u ((a `f` b) `f` c))

-- | Tests whether a particular operation is commutative, that is:
--
-- > a `f` b == b `f` a
commutative
  :: (Eq b, Show b)
  => Gen (a -> a -> a)
  -- ^ Generates a commutative operation
  -> Gen (a -> b)
  -- ^ Generates unwrappers
  -> Gen a
  -- ^ Generates values
  -> Gen Property
commutative gF gU gV = do
  f <- gF
  u <- gU
  a <- gV
  b <- gV
  return $ (u (a `f` b)) === (u (b `f` a))

-- | Tests whether a particular value is the left identity, that is:
--
-- > z `f` a == a
leftIdentity
  :: (Eq b, Show b)
  => Gen (a -> a -> a)
  -- ^ Generates the operation to test
  -> Gen (a -> b)
  -- ^ Generates unwrappers
  -> Gen a
  -- ^ Generates identity values
  -> Gen a
  -- ^ Generates right-hand side values
  -> Gen Property
leftIdentity gF gU gZ gR = do
  f <- gF
  u <- gU
  z <- gZ
  r <- gR
  return $ u (z `f` r) === u r

-- | Tests whether a particular value is the right identity, that is:
--
-- > a `f` z == a
rightIdentity
  :: (Eq b, Show b)
  => Gen (a -> a -> a)
  -- ^ Generates the operation to test
  -> Gen (a -> b)
  -- ^ Generates unwrappers
  -> Gen a
  -- ^ Generates identity values
  -> Gen a
  -- ^ Generates left-hand side values
  -> Gen Property
rightIdentity gF gU gZ gL = do
  f <- gF
  u <- gU
  z <- gZ
  l <- gL
  return $ u (l `f` z) === u l
