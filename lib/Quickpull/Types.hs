{-# LANGUAGE ExistentialQuantification #-}
module Quickpull.Types where

-- | A tree of tests.  This allows you to group tests for convenient
-- display; it also allows you to write computations that return
-- entire groups of tests.

import Quickpull.Formatting
import Test.QuickCheck
import System.FilePath

data TestTree = TestTree
  { label :: String
  , payload :: Node
  }

instance Show TestTree where
  show = go 0
    where
      go d (TestTree s n) = case n of
        Group ts -> indent d ("group: " ++ s)
          ++ concatMap (go (d + 1)) ts
        Test _ -> indent d ("test: " ++ s)

data Node
  = Group [TestTree]
  -- ^ A group of tests.  Contains a list of 'TestTree', each of which
  -- might be a 'Test' or another 'Group'.

  | forall a. Testable a => Test a
  -- ^ A QuickCheck test to run.

-- | A single property or tree to test.
data Item
  = forall a. Testable a => Single a
  | Multi TestTree

-- | Metadata about a particular test or group.
data Meta = Meta
  { modDesc :: ModDesc
  -- ^ Name of file providing this test or group.

  , linenum :: Int
  -- ^ Line number of the test or group.

  , qName :: String
  -- ^ The name of the test or group, such as @prop_mytest@ or
  -- @proptree_mytest@.

  } deriving (Eq, Ord, Show)

-- | Description of a single property or tree to test from a file.
-- Unlike 'Item', this does not contain the actual item; this is for
-- use when parsing a test input module.
data Qual= QTree | QProp
  deriving (Eq, Ord, Show)


-- | Description of a module.
data ModDesc = ModDesc
  { modPath :: String
  -- ^ Path to the module

  , modName :: [String]
  -- ^ Each part of the hierarchical name
  } deriving (Eq, Ord, Show)

-- | Creates a 'ModDesc'.
makeModDesc
  :: FilePath
  -- ^ Reading was started in this directory

  -> [FilePath]
  -- ^ Directory stack

  -> FilePath
  -- ^ Name of specific file

  -> ModDesc

makeModDesc strt ds fln = ModDesc
  (joinPath $ strt : reverse ds ++ [fln])
  (reverse ds ++ [takeWhile (/= '.') fln])


-- | Specifies a single item to test, along with metadata about that
-- item.
data Decree = Decree
  { meta :: Meta
  , item :: Item
  }

-- | Summary of all QuickCheck results.
data Summary = Summary
  { success :: !Int
  , gaveUp :: !Int
  , failure :: !Int
  , noExpectedFailure :: !Int
  }
