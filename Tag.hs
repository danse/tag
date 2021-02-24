module Tag where

import System.FilePath.Posix (splitPath)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix (splitDirectories)
import System.FilePath.Glob (namesMatching)
import Data.Set (Set, union)
import qualified Data.Map.Strict as Map
import qualified Data.Set  as Set
import qualified Data.List as List
import Data.Map.Strict (
  Map,
  fromListWith,
  toList,
  unionsWith)
import Data.Tuple (swap)

type Tag    = String
type Tagged = String

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
  
{-
-- new data models
data Graph n = Graph [Edge n] deriving Show
data Edge  n = Edge (n, n) deriving Show -- from, to
data GraphByFrom n = GraphByFrom (Map n (Set n)) deriving Show
data GraphByTo   n = GraphByTo   (Map n (Set n)) deriving Show
data Linked n = Linked (n, [Linked n])
-}

-- Branch is for mapping the file configuration
data Branch = Branch { tagPath :: FilePath, contentPaths :: [FilePath] } deriving Show

walk :: IO [Branch]
walk = do
  paths <- collectBranches
  sequence (Prelude.map walkBranch paths)
    where walkBranch :: FilePath -> IO Branch
          walkBranch tagPath' = do
            contentPaths' <- contained tagPath'
            pure (Branch tagPath' contentPaths')
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
        addPath t p = (readTag p, Set.singleton $ readTag t)
      linkedToMap :: [(String, Set String)] -> Map String (Set String)
      linkedToMap = Data.Map.Strict.fromListWith union

invertGraph :: Map Tagged (Set Tag) -> Map Tag (Set Tagged)
invertGraph = Map.fromListWith union . expand . fmap swap . toList
  where expand [] = []
        expand ((s, tagged):r) = fmap (\tag -> (tag, Set.singleton tagged)) (Set.toList s) ++ expand r
        expand :: [(Set Tag, Tagged)] -> [(Tag, Set Tagged)]

data TaggedSet = TaggedSet { taggedSetTag :: Tag, taggedSet :: Set Tagged }

subsets :: [TaggedSet] -> TaggedSet -> [TaggedSet]
subsets candidates (TaggedSet _ s1) =
  let filtering :: TaggedSet -> Bool
      filtering (TaggedSet _ s2) = s2 `Set.isProperSubsetOf` s1
  in List.filter filtering candidates

graphToTagged  :: Map Tag (Set Tagged) -> [TaggedSet]
graphToTagged = fmap (uncurry TaggedSet) . Map.toList

-- | Try reading a tag file structure from the current directory or
-- show an error
readInvertedGraph :: IO (Map Tag (Set Tagged))
readInvertedGraph = fmap (invertGraph . parseGraph) walk
