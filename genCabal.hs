-- Generate Cabal file using the Cartel library.
-- Written for Cartel version 0.10.0.2

module Main where

import qualified Cartel as C

version :: [Int]
version = [0,2,0,0]

quickcheck :: C.Package
quickcheck = C.closedOpen "QuickCheck" [2,7,2] [2,8]

base :: C.Package
base = C.closedOpen "base" [4,5,0,0] [4,8]

directory :: C.Package
directory = C.closedOpen "directory" [1,1,0,2] [1,3]

filepath :: C.Package
filepath = C.closedOpen "filepath" [1,3,0,0] [1,4]

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
    ]
  }

ghcOptions :: [String]
ghcOptions = ["-Wall"]

library
  :: [String]
  -- ^ Library modules
  -> C.Library
library ms = C.Library
  [ C.buildDepends
    [ base
    , quickcheck
    , filepath
    , directory
    ]
  , C.defaultLanguage C.Haskell2010
  , C.hsSourceDirs ["lib"]
  , C.ghcOptions ghcOptions
  , C.LibExposedModules ms
  ]

executable :: C.Executable
executable = C.Executable "quickpull"
  [ C.buildDepends
    [ base
    , quickcheck
    , directory
    , filepath
    ]
  , C.defaultLanguage C.Haskell2010
  , C.hsSourceDirs ["lib", "bin"]
  , C.ExeMainIs "quickpull.hs"
  , C.ghcOptions ghcOptions
  ]

cabal :: [String] -> C.Cabal
cabal ms = C.empty
  { C.cProperties = properties
  , C.cLibrary = Just $ library ms
  , C.cExecutables = [ executable ]
  }

main :: IO ()
main = do
  ms <- C.modules "lib"
  C.render "genCabal.hs" $ cabal ms
