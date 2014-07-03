{-# LANGUAGE ExistentialQuantification #-}
module Quickpull where

import Test.QuickCheck
import Quickpull.Types

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

quickcheckTree
  :: Testable prop
  => (prop -> IO Result)
  -> TestTree
  -> IO [Result]
quickcheckTree = undefined
