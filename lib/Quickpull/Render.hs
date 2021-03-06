module Quickpull.Render where

import Quickpull.Types
import Quickpull.Formatting
import Data.List
import Data.Ord

metaQual
  :: Char
  -- ^ Leader character

  -> (Meta, Qual)
  -> String

metaQual ldr (m, q) = indent 1 $ [ldr] <+> "Decree (" <+>
  show m <+> ")" <+> "(" <+> i <+> ")"
  where
    kind = case q of
      QTree -> "Multi"
      QProp -> "Single"
    i = kind <+> qualName
    qualName = (concat . intersperse "." . modName . modDesc $ m)
      ++ "." ++ qName m

metaQuals :: [(Meta, Qual)] -> String
metaQuals ls = case ls of
  [] -> indent 1 "[]"
  x:xs -> metaQual '[' x ++ concatMap (metaQual ',') xs
    ++ indent 1 "]"

imports :: [ModDesc] -> String
imports = concatMap mkImport . nub . sortBy (comparing modName)
  where
    mkImport m = "import qualified " ++
      (concat . intersperse "." . modName $ m) ++ "\n"

-- | Summarizes a Meta in a single line.
metaLine :: Meta -> String
metaLine m = qName m <+> "from file" <+> (modPath . modDesc $ m)
  <+> "at line" <+> show (linenum m) ++ "\n"

topComments :: String
topComments = unlines
  [ "-- | This module generated by the Quickpull package."
  , "-- Quickpull is available at:"
  , "-- <http://www.github.com/massysett/quickpull>"
  , ""
  ]

testModule
  :: String
  -- ^ Name to use for module
  -> [(Meta, Qual)]
  -> String
testModule name ls = topComments ++ rest
  where
    rest =  concat . intersperse "\n" $
      [ "module" <+> name <+> "where\n"
      , "import Quickpull"
      , imports . map (modDesc . fst) $ ls
      , unlines
        [ "decrees :: [Decree]"
        , "decrees ="
        ]
      , metaQuals ls
      ]

summary :: Summary -> String
summary s = unlines
  [ "success: " ++ show (success s)
  , "gave up: " ++ show (gaveUp s)
  , "failure: " ++ show (failure s)
  , "no expected failure: " ++ show (noExpectedFailure s)
  , "total: " ++ show
    (success s + gaveUp s + failure s +
     noExpectedFailure s)
  ]
