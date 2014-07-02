module Main where

import System.Directory
import System.FilePath
import Data.List
import Data.Char

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
  

main :: IO ()
main = undefined
