{-# LANGUAGE RankNTypes #-}
module Quickpull.Runners where

import Test.QuickCheck
  ( Testable, Result(..), quickCheckResult, quickCheckWithResult,
    Args )
import Quickpull.Types
import Quickpull.Formatting
import Quickpull.Render
import Data.List (foldl')
import System.Exit
import Control.Applicative

quickcheckTree
  :: (forall a. Testable a => a -> IO Result)
  -> TestTree
  -> IO [Result]
quickcheckTree run = go 0
  where
    go lvl (TestTree l n) = do
      putStr . indent lvl $ l
      case n of
        Group tt -> fmap concat . mapM (go (succ lvl)) $ tt
        Test t -> do
          putStr (replicate (indentAmt * (succ lvl)) ' ')
          fmap (:[]) $ run t

quickcheckDecree
  :: (forall a. Testable a => a -> IO Result)
  -> Decree
  -> IO [Result]
quickcheckDecree run (Decree m i) = do
  putStr . metaLine $ m
  case i of
    Single a -> fmap (:[]) . run $ a
    Multi t -> quickcheckTree run t


summarize :: [Result] -> Summary
summarize = foldl' f (Summary 0 0 0 0)
  where
    f s r = case r of
      Success {} -> s { success = succ (success s) }
      GaveUp {} -> s { gaveUp = succ (gaveUp s) }
      Failure {} -> s { failure = succ (failure s) }
      NoExpectedFailure {} -> s
        { noExpectedFailure = succ (noExpectedFailure s) }

exitCode :: Summary -> ExitCode
exitCode s
  | gaveUp s == 0 && failure s == 0 &&
    noExpectedFailure s == 0 = ExitSuccess
  | otherwise = ExitFailure 1

withDecree
  :: (Decree -> forall a. Testable a => a -> IO Result)
  -> [Decree]
  -> IO [[Result]]
withDecree f ds = mapM g ds
  where
    g d = quickcheckDecree (f d) d <* putStrLn ""

runTests
  :: (Decree -> forall a. Testable a => a -> IO Result)
  -> [Decree]
  -> IO ()
runTests f ds = do
  rs <- fmap concat $ withDecree f ds
  let s = summarize rs
      c = exitCode s
  putStr $ summary s
  exitWith c

defaultMain :: [Decree] -> IO ()
defaultMain = runTests (const quickCheckResult)

defaultMainWith :: Args -> [Decree] -> IO ()
defaultMainWith a = runTests (const (quickCheckWithResult a))
