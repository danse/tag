module Tag where

import System.FilePath.Posix (splitPath)
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
  fromList,
  size)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict (
  empty,
  filter,
  fromListWith,
  map,
  Map (..),
  singleton,
  toList,
  unionsWith,
  unionWith)
import Data.Tuple (swap)

type Tag    = String
type Tagged = String

-- this currently fails on paths ending with "/"
takePathEnd :: FilePath -> FilePath
takePathEnd = head . dropWhile null . reverse . splitPath

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
        addPath tag p = (readTag p, Set.singleton $ readTag tag)
      linkedToMap :: [(String, Set String)] -> Map String (Set String)
      linkedToMap = Data.Map.Strict.fromListWith union

invertGraph :: Map Tagged (Set Tag) -> Map Tag (Set Tagged)
invertGraph = Map.fromListWith union . expand . fmap swap . toList
  where expand [] = []
        expand ((s, tagged):r) = fmap (\tag -> (tag, Set.singleton tagged)) (Set.toList s) ++ expand r
        expand :: [(Set Tag, Tagged)] -> [(Tag, Set Tagged)]

select :: [String]
       -> [String]
       -> Maybe Int
       -> Maybe Int
       -> Bool
       -> [Branch]
       -> Map String (Set String)
select i x min max o =
  let p1 :: Set String -> Bool
      p1 s
        | null i    = True
        | otherwise = (size (fromList i) == size (intersection (fromList i) s))
      p2 = null . intersection (fromList x)
      p3 s = maybe True (\ card -> size s >= card) min
      p4 s = maybe True (\ card -> size s <= card) max
      pred :: Set String -> Bool
      pred s
        | o         = or  partialPredicates
        | otherwise = and partialPredicates
        where partialPredicates = [p1 s, p2 s, p3 s, p4 s]
  in Data.Map.Strict.filter pred . parseGraph

