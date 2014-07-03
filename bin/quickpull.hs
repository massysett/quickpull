module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Quickpull.Files
import Quickpull.Render

help
  :: String
  -- ^ Program name
  -> String

help pn = unlines
  [ "usage: " ++ pn ++ " DIRECTORY..."
  , "Finds each Haskell module in each given directory."
  , "Prints to standard output a module containing a list"
  , "of all QuickCheck properties in each module."
  ]

runnerModuleName :: String
runnerModuleName = "AllProperties"

main :: IO ()
main = do
  as <- getArgs
  pn <- getProgName
  when (as == ["-h"] || as == ["--help"]) $ do
    hPutStr stderr $ help pn
    exitSuccess
  mds <- fmap concat . mapM allModules $ as
  ps <- fmap concat . mapM readAndParse $ mds
  putStr $ testModule runnerModuleName ps
