{-# LANGUAGE ExistentialQuantification #-}
module Quickpull
  ( TestTree(..)
  , Node(..)
  , group
  , test
  , quickcheckTree
  ) where

import Test.QuickCheck

-- | A tree of tests.  This allows you to group tests for convenient
-- display; it also allows you to write computations that return
-- entire groups of tests.

data TestTree = TestTree
  { label :: String
  , payload :: Node
  }

data Node
  = Group [TestTree]
  -- ^ A group of tests.  Contains a list of 'TestTree', each of which
  -- might be a 'Test' or another 'Group'.

  | forall a. Testable a => Test a
  -- ^ A QuickCheck test to run.

-- | Create a new 'Group' of tests.
group
  :: String
  -- ^ Group name; a string with no trailing newline.

  -> [TestTree]
  -> TestTree
group n ts = TestTree n (Group ts)

-- | Create a new 'Test'.
test
  :: Testable a

  => String
  -- ^ Test name; a string with no trailing newline.

  -> a
  -> TestTree
test n t = TestTree n (Test t)

indent :: Int -> String -> String
indent d s = replicate (d * 2) ' ' ++ s ++ "\n"

instance Show TestTree where
  show = go 0
    where
      go d (TestTree s n) = case n of
        Group ts -> indent d ("group: " ++ s)
          ++ concatMap (go (d + 1)) ts
        Test _ -> indent d ("test: " ++ s)

quickcheckTree
  :: Testable prop
  => (Args -> prop -> IO Result)
  -> TestTree
  -> IO [Result]
quickcheckTree = undefined
