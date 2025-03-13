module Tag where

import System.FilePath.Posix (splitPath)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix (splitDirectories)
import System.FilePath.Glob (namesMatching)

import Data.Hypergraph
import Data.Map.Strict (
  Map,
  fromListWith,
  toList,
  unionsWith)
import Data.Set (Set, union)
import Data.Tuple (swap)

import qualified Data.Map.Strict as Map
import qualified Data.Set  as Set
import qualified Data.List as List

newtype Tag = Tag String deriving (Eq, Ord, Show)
type Tagged = Hypergraph Tag

-- this currently fails on paths ending with "/"
takePathEnd :: FilePath -> FilePath
takePathEnd = head . dropWhile null . reverse . splitPath

rootDir :: [Char]
rootDir = "tags"

-- an easy alternative is to get roots from the home with
-- `System.Directory.getHomeDirectory` as i used to do before in the
-- filter
localTagsDir :: IO FilePath
localTagsDir = do
  curr <- getCurrentDirectory
  pure (curr ++ "/" ++ rootDir)

checkRootDir :: Maybe FilePath -> IO FilePath
checkRootDir maybePath = do
  local <- localTagsDir
  pure (maybe local addSlashIfMissing maybePath)
    where addSlashIfMissing = id
  
-- Branch is for mapping the filesystem structure
data Branch =
  Branch {
    tagPath      :: FilePath,  -- ^ a folder, corresponding to a tag
    contentPaths :: [FilePath] -- ^ folder contents, that duplicate in
                               -- the different folders
    } deriving Show

readBranches :: IO [Branch]
readBranches = do
  paths <- collectBranches
  sequence (Prelude.map readBranch paths)
    where readBranch :: FilePath -> IO Branch
          readBranch tagPath' = do
            contentPaths' <- contained tagPath'
            pure (Branch tagPath' contentPaths')
          collectBranches :: IO [FilePath]
          collectBranches = do
            home <- getCurrentDirectory
            contained (home ++ "/" ++ rootDir)
          contained :: FilePath -> IO [String]
          contained dir = namesMatching (dir ++ "/*")

parseGraph :: [Branch] -> Tagged String
parseGraph = Hypergraph . Data.Map.Strict.unionsWith union . fmap branchToMap
    where
      -- works on "a/b/" and "a/b"
      lastSplit :: FilePath -> String
      lastSplit = last . splitDirectories
      readTag :: FilePath -> Tag
      readTag = Tag . lastSplit
      branchToMap = linkedToMap . branchToLinked
      branchToLinked :: Branch -> [(String, Set Tag)]
      branchToLinked (Branch tag contents) =
        fmap (addContent tag) contents
        where addContent t c = (lastSplit c, Set.singleton $ readTag t)
      linkedToMap :: [(String, Set Tag)] -> Map String (Set Tag)
      linkedToMap = Data.Map.Strict.fromListWith union

data TaggedSet = TaggedSet { taggedSetTag :: Tag, taggedSet :: Set String }

subsets :: [TaggedSet] -> TaggedSet -> [TaggedSet]
subsets candidates (TaggedSet _ s1) =
  let filtering :: TaggedSet -> Bool
      filtering (TaggedSet _ s2) = s2 `Set.isProperSubsetOf` s1
  in List.filter filtering candidates

graphToTagged  :: Map Tag (Set String) -> [TaggedSet]
graphToTagged = fmap (uncurry TaggedSet) . Map.toList

-- | Try reading a tag file structure from the current folder
readGraph :: IO (Tagged String)
readGraph = parseGraph <$> readBranches
