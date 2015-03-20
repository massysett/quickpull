-- Generate Cabal file using the Cartel library.
-- Written for Cartel version 0.10.0.2

module Main where

import qualified Cartel as C

version :: [Int]
version = [0,4,2,2]

-- Dependencies are intended to work with GHC 7.4.1.  Versions that
-- came with GHC 7.4.1:
-- base-4.5.0.0
-- directory-1.1.0.2
-- filepath-1.3.0.0

-- Versions that came with GHC 7.8.3:
-- base-4.7.0.1
-- directory-1.2.1.0
-- filepath-1.3.0.2

-- quickcheck needs to be at least 2.7, as that version has the ===
-- combinator.  As of 2014-07-05 the oldest version in the 2.7 series
-- that is on Hackage is 2.7.2.

base :: C.Package
base = C.closedOpen "base" [4,5,0,0] [4,8]

quickcheck :: C.Package
quickcheck = C.closedOpen "QuickCheck" [2,7,2] [2,9]

directory :: C.Package
directory = C.closedOpen "directory" [1,1,0,2] [1,3]

filepath :: C.Package
filepath = C.closedOpen "filepath" [1,3,0,0] [1,5]

barecheck :: C.Package
barecheck = C.closedOpen "barecheck" [0,2,0,6] [0,3]

commonOptions :: C.Field a => [a]
commonOptions =
  [ C.buildDepends
    [ base
    , directory
    , filepath
    ]
  , C.cif (C.flag "old-quick-check")
    [ C.buildDepends [C.Package "QuickCheck" (Just (C.lt [2,7]))]
    , C.hsSourceDirs ["quickcheck-old"]
    ]
    [ C.buildDepends [ C.closedOpen "QuickCheck" [2,7] [2,8] ]
    , C.hsSourceDirs ["quickcheck-new"]
    ]
  , C.hsSourceDirs ["lib"]
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  ]


properties :: C.Properties
properties = C.empty
  { C.prName = "quickpull"
  , C.prVersion = C.Version version
  , C.prLicenseFile = "LICENSE"
  , C.prCopyright = "Copyright 2014 Omari Norman"
  , C.prAuthor = "Omari Norman, omari@smileystation.com"
  , C.prMaintainer = "omari@smileystation.com"
  , C.prStability = "Experimental"
  , C.prHomepage = "http://www.github.com/massysett/quickpull"
  , C.prBugReports = "http://www.github.com/massysett/quickpull/issues"
  , C.prSynopsis = "Generate Main module with QuickCheck tests"
  , C.prCategory = "Testing"
  , C.prDescription =
    [ "Reads a tree of modules and outputs a module to run all"
    , "QuickCheck tests."
    , "For more information, please see documentation in the"
    , "\"Quickpull\" module."
    ]
  , C.prExtraSourceFiles =
    [ "genCabal.hs"
    , "README.md"
    , "quickcheck-new/Quickpull/EqShow.hs"
    , "quickcheck-old/Quickpull/EqShow.hs"
    ]
  }

ghcOptions :: [String]
ghcOptions = ["-Wall"]

library
  :: [String]
  -- ^ Library modules
  -> C.Library
library ms = C.Library $ commonOptions ++
  [ C.LibExposedModules $ "Quickpull.EqShow" : ms
  ]

testSuite :: C.TestSuite
testSuite = C.TestSuite "quickpull-tests" $ commonOptions ++
  [ C.TestType C.ExitcodeStdio
  , C.TestMainIs "quickpull-tests.hs"
  , C.otherModules ["Decrees", "Tests"]
  , C.hsSourceDirs ["tests"]
  ]


executable :: C.Executable
executable = C.Executable "quickpull" $ commonOptions ++
  [ C.hsSourceDirs ["bin"]
  , C.ExeMainIs "quickpull-main.hs"
  ]

-- This tests the Gen monad of QuickCheck.  Not included in the
-- default tests because it will fail, as Gen is not a true monad.
exeTestGen :: C.Executable
exeTestGen = C.Executable "quickpull-test-gen"
  [ C.cif (C.flag "build-test-gen")
    commonOptions
    [ C.buildable False ]
  , C.buildDepends [ barecheck ]
  , C.ExeMainIs "quickpull-test-gen.hs"
  , C.hsSourceDirs ["quickcheck-tests"]
  , C.otherModules ["Decrees", "Tests"]
  ]

flagTestGen :: C.Flag
flagTestGen = C.empty
  { C.flName = "build-test-gen"
  , C.flDescription = "Build the quickpull-test-gen executable"
  , C.flDefault = False
  , C.flManual = True
  }

flagOldQuickCheck :: C.Flag
flagOldQuickCheck = C.empty
  { C.flName = "old-quick-check"
  , C.flDescription = "Use version of QuickCheck before version 2.7"
  , C.flDefault = False
  , C.flManual = False
  }

repo :: C.Repository
repo = C.empty
  { C.repoVcs = C.Git
  , C.repoKind = C.Head
  , C.repoLocation = "https://github.com/massysett/quickpull.git"
  }

cabal :: [String] -> C.Cabal
cabal ms = C.empty
  { C.cProperties = properties
  , C.cRepositories = [repo]
  , C.cLibrary = Just $ library ms
  , C.cExecutables = [ executable, exeTestGen ]
  , C.cTestSuites = [testSuite]
  , C.cFlags = [ flagTestGen, flagOldQuickCheck ]
  }

main :: IO ()
main = do
  ms <- C.modules "lib"
  C.render "genCabal.hs" $ cabal ms
