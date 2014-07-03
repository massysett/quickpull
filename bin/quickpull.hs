module Main where

import System.Directory
import System.FilePath
import Data.List
import Data.Char
import Data.Ord
import Data.Maybe

data ModDesc = ModDesc
  { modPath :: String
  -- ^ Path to the module

  , modName :: [String]
  -- ^ Each part of the hierarchical name
  } deriving (Eq, Ord, Show)

-- | Creates a 'ModDesc'.
modDesc
  :: FilePath
  -- ^ Reading was started in this directory

  -> [FilePath]
  -- ^ Directory stack

  -> FilePath
  -- ^ Name of specific file

  -> ModDesc

modDesc strt ds fln = ModDesc
  (joinPath $ strt : reverse ds ++ [fln])
  (reverse ds ++ [takeWhile (/= '.') fln])


-- | Pulls all modules from the given directory.  A module is any file
-- that begins with a capital letter and ends in @.hs@.
allModules
  :: FilePath
  -- ^ Start reading at this directory

  -> IO [ModDesc]

allModules = modsInDirectory []

-- | Takes a stack of directories and a directory where reading was
-- started; returns a single path to the relevant directory.

concatDirs :: FilePath -> [FilePath] -> FilePath
concatDirs st rst = joinPath $ st : reverse rst

isInterestingFile :: FilePath -> Bool
isInterestingFile p = case p of
  [] -> False
  x:xs -> isUpper x && ".hs" `isSuffixOf` xs

isInterestingDir :: FilePath -> Bool
isInterestingDir p = case p of
  [] -> False
  x:_ -> isUpper x


-- | Pulls all modules from a given directory.
modsInDirectory
  :: [FilePath]
  -- ^ Stack of directories.  When descending through a tree, add new
  -- directories to the head of this list.

  -> FilePath
  -- ^ Reading was started in this directory

  -> IO [ModDesc]
modsInDirectory stk strt = do
  filesAndDirs <- fmap (filter (\x -> x /= "." && x /= ".."))
    . getDirectoryContents $ concatDirs strt stk
  bools <- mapM doesDirectoryExist filesAndDirs
  let ps = zip bools filesAndDirs
      dirs = filter isInterestingDir . map snd . filter fst $ ps
      files = filter isInterestingFile . map snd
        . filter (not . fst) $ ps
      mods = map (modDesc strt stk) files
      readDir dirName = modsInDirectory (dirName : stk) strt
  subdirs <- fmap concat . mapM readDir $ dirs
  return $ mods ++ subdirs

-- | A single property or tree to test from a file.
data Qual = QTree | QProp
  deriving (Eq, Ord, Show)


-- | A 'Qual' bundled up with other information about the 'Qual'.
data Qinfo = Qinfo
  { qModDesc :: ModDesc
  -- ^ Name of file providing the 'Qual'.

  , linenum :: Int
  -- ^ Line number of the 'Qual'

  , qual :: Qual

  , qName :: String
  -- ^ The name of the 'Qual', such as @prop_mytest@ or
  -- @proptree_mytest@.

  } deriving (Eq, Ord, Show)


-- | Pulls all properties from the text of a file.  Properties that
-- are 'Testable' must begin with @prop_@.  Properties that are a
-- 'TestTree' must begin with @proptree_@.
--
-- Steps in this computation:
--
-- * Split text into lines
--
-- * Add line numbers
--
-- * Extract all lines that have a word in the first column; keep only
-- this first word and discard the rest of the line
--
-- * Sort list by this first word, then reverse the list so that the
-- second occurrence of the word is earlier in the list.  This way, if
-- there is a type signature followed by a definition, the type
-- signature is discarded in the next step.
--
-- * use 'nubBy' to remove duplicate words, such as type signatures
--
-- * sort words by line number order, so they're in original order again
--
-- * Create 'Qinfo'; here, words that do not start with @prop_@ or
-- @proptree_@ are discarded

getQuals
  :: ModDesc
  -> String
  -- ^ Module text
  -> [Qinfo]
getQuals d
  = mapMaybe mkQ
  . sortBy (comparing fst)
  . nubBy (\x y -> snd x == snd y)
  . reverse
  . sortBy (comparing snd)
  . mapMaybe getFirstWord
  . zip [1..]
  . lines
  where
    mkQ (i, var)
      | "prop_" `isPrefixOf` var = Just $ Qinfo d i QProp var
      | "proptree_" `isPrefixOf` var = Just $ Qinfo d i QTree var
      | otherwise = Nothing
    getFirstWord (i, s)
      | null r = Nothing
      | otherwise = Just (i, r)
      where
        r = takeWhile varIdChar s
        varIdChar c = isLower c || isUpper c || isDigit c
          || c == '_' || c == '\''
    

main :: IO ()
main = undefined
