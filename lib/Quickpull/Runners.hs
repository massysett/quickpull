{-# LANGUAGE RankNTypes #-}
-- | Running tests.  To check a 'TestTree' in a REPL, use
-- 'quickCheckTree'; to check a QuickCheck 'Testable', just use
-- 'Test.QuickCheck.quickCheck' and the similar functions in
-- "Test.QuickCheck".  The other functions in this module will be more
-- useful for checking from within a compiled test program.
--
-- The QuickCheck docs in "Test.QuickCheck" contain warnings about how
-- type defaulting in GHCi can cause types to silently default to
-- @()@; all those warnings also apply to use of the functions in this
-- module.  Although QuickCheck has some Template Haskell to help with
-- this, Quickpull currently does not.  You don't have to worry about
-- this in non-interactive use, as the defaulting rules are more
-- strict and, with warnings on, GHC will warn you when it defaults a
-- type.
module Quickpull.Runners
  ( -- * Testing 'TestTree'
    quickCheckTree
  , treeWithResult

    -- * Testing 'Decree's
  , decreeWithResult
  , seeDecree

  -- * Default main functions
  , defaultMain
  , defaultMainWith

  -- * Summarizing
  , summarize
  , exitCode
  ) where

import Test.QuickCheck
  ( Testable, Result(..), quickCheckResult, quickCheckWithResult,
    Args )
import Quickpull.Types
import Quickpull.Formatting
import Quickpull.Render
import Data.List (foldl')
import System.Exit
import Control.Applicative

-- | Checks a 'TestTree' and prints the result to standard output in
-- addition to returning it as a list of 'Result'.  Each 'Decree'
-- returns a list of 'Result' (a 'Single' returns a single 'Result',
-- while a 'Multi' returns a 'Result' for each test in the tree.)
treeWithResult
  :: (forall a. Testable a => a -> IO Result)
  -> TestTree
  -> IO [Result]
treeWithResult run = go 0
  where
    go lvl (TestTree l n) = do
      putStr . indent lvl $ l
      case n of
        Group tt -> fmap concat . mapM (go (succ lvl)) $ tt
        Test t -> do
          putStr (replicate (indentAmt * (succ lvl)) ' ')
          fmap (:[]) $ run t

-- | Checks a 'TestTree' and prints the result to standard output.
-- Intended for use in a REPL; however, the QuickCheck docs in
-- "Test.QuickCheck" contain warnings about how type defaulting in
-- GHCi can cause types to silently default to @()@; all those
-- warnings also apply to use of this function.
quickCheckTree :: TestTree -> IO ()
quickCheckTree t = do
  _ <- treeWithResult quickCheckResult t
  return ()

-- | Tests a 'Decree' and prints the result to standard output in
-- addition to returning a list of 'Result'.  Each 'Decree' returns a
-- list of 'Result' (a 'Single' returns a single 'Result', while a
-- 'Multi' returns a 'Result' for each test in the tree.)
decreeWithResult
  :: (forall a. Testable a => a -> IO Result)
  -> Decree
  -> IO [Result]
decreeWithResult run (Decree m i) = do
  putStr . metaLine $ m
  case i of
    Single a -> fmap (:[]) . run $ a
    Multi t -> treeWithResult run t


-- | Tallies up the 'Result's.
summarize :: [Result] -> Summary
summarize = foldl' f (Summary 0 0 0 0)
  where
    f s r = case r of
      Success {} -> s { success = succ (success s) }
      GaveUp {} -> s { gaveUp = succ (gaveUp s) }
      Failure {} -> s { failure = succ (failure s) }
      NoExpectedFailure {} -> s
        { noExpectedFailure = succ (noExpectedFailure s) }

-- | Exit successfully if there were no failures, give-ups, or
-- no-expected-failures; otherwise, exit unsuccessfully.
exitCode :: Summary -> ExitCode
exitCode s
  | gaveUp s == 0 && failure s == 0 &&
    noExpectedFailure s == 0 = ExitSuccess
  | otherwise = ExitFailure 1

-- | Tests each 'Decree' using a custom function that you specify;
-- this allows you to vary the test depending on what's in the
-- 'Decree'.  Each 'Decree' returns a list of 'Result' (a 'Single'
-- returns a single 'Result', while a 'Multi' returns a 'Result' for
-- each test in the tree.)  The tests are printed to standard output
-- as they run, in addition to returning the 'Result'.
seeDecree
  :: (Decree -> forall a. Testable a => a -> IO Result)
  -> [Decree]
  -> IO [[Result]]
seeDecree f ds = mapM g ds
  where
    g d = decreeWithResult (f d) d <* putStrLn ""

testDecrees
  :: (Decree -> forall a. Testable a => a -> IO Result)
  -> [Decree]
  -> IO ()
testDecrees f ds = do
  rs <- fmap concat $ seeDecree f ds
  let s = summarize rs
      c = exitCode s
  putStr $ summary s
  exitWith c

-- | Tests each 'Decree' and prints the results to standard output.
-- Exits successfully if all tests succeeded; otherwise, exits
-- unsuccessfully.
--
-- Not recommended for REPL use as this function will either kill your
-- REPL when it's done or, in the case of recent GHC versions, issue
-- an exception.
defaultMain :: [Decree] -> IO ()
defaultMain = testDecrees (const quickCheckResult)

-- | Like 'defaultMain' but allows you to pass arguments to the
-- QuickCheck driver.
--
-- Not recommended for REPL use as this function will either kill your
-- REPL when it's done or, in the case of recent GHC versions, issue
-- an exception.
defaultMainWith :: Args -> [Decree] -> IO ()
defaultMainWith a = testDecrees (const (quickCheckWithResult a))
