{-# LANGUAGE RecordWildCards #-}
import System.Directory (
  createDirectoryIfMissing,
  getCurrentDirectory,
  setCurrentDirectory)
import System.FilePath.Posix (
  takeDirectory,
  takeFileName,
  splitDirectories,
  splitPath)
import System.FilePath.Glob (namesMatching)
import Data.Set (
  Set (..),
  intersection,
  difference,
  union,
  unions,
  singleton,
  fromList)
import Data.Map.Strict (
  Map (..),
  map,
  filter,
  singleton,
  unionWith,
  unionsWith,
  fromListWith,
  empty,
  toList)
import Data.Functor ((<$>))
import Control.Monad (join)
import Options.Applicative
import Tag

-- Branch is for mapping the file configuration
data Branch = Branch { tagPath :: FilePath, contentPaths :: [FilePath] } deriving Show

walk :: IO [Branch]
walk = do
  paths <- collectBranches
  sequence (Prelude.map walkBranch paths)
    where walkBranch :: FilePath -> IO Branch
          walkBranch path = do
            contentPaths <- contained path
            pure (Branch path contentPaths)
          collectBranches :: IO [FilePath]
          collectBranches = do
            home <- getCurrentDirectory
            contained (home ++ "/" ++ rootDir)
          contained :: FilePath -> IO [String]
          contained dir = namesMatching (dir ++ "/*")

parseGraph :: [Branch] -> Map String (Set String)
parseGraph = Data.Map.Strict.unionsWith union . fmap branchToMap
    where
      -- works on "a/b/" and "a/b"
      readTag :: FilePath -> Tag
      readTag = last . splitDirectories 
      branchToMap = linkedToMap . branchToLinked
      branchToLinked :: Branch -> [(String, Set String)]
      branchToLinked (Branch tag paths) = fmap (addPath tag) paths where
        addPath tag p = (readTag p, Data.Set.singleton $ readTag tag)
      linkedToMap :: [(String, Set String)] -> Map String (Set String)
      linkedToMap = Data.Map.Strict.fromListWith union

select :: Options -> [Branch] -> Map String (Set String)
select (Options m) =
  let predicate :: Set String -> Bool
      predicate = not . null . intersection (fromList m)
  in Data.Map.Strict.filter predicate . parseGraph

data Options = Options {
  matching :: [String]
  }

optionParser :: Parser Options
optionParser = Options
               <$> some (strOption (long "match" <> short 'm'))

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

main = do
  opts <- execParser optionParserInfo
  branches <- walk
  print (select opts branches)

-- filtering logic modeled after a nand in order to narrow strictly
--p :: Set String -> Set String -> Set String -> Bool
--p all focused = null . intersection (difference all focused)

