{-# LANGUAGE RecordWildCards #-}
import System.Directory (createDirectoryIfMissing, copyFile, removeFile, getCurrentDirectory, getHomeDirectory)
import System.FilePath.Posix (takeDirectory, takeFileName, splitDirectories, splitPath)
import System.FilePath.Glob (namesMatching)
import Data.Set (Set (..), intersection, difference, union, unions, singleton, fromList)
import Data.Map.Strict (Map (..), map, singleton, unionWith, unionsWith, fromListWith, empty, toList)
import Options.Applicative
import Tag

type Title = String

type Tags = Set Tag
type TagMap = Map Title Tags
data Tagged a b = Tagged { content :: a, features :: Set b } deriving Show
-- Branch is for mapping the file configuration
data Branch = Branch { tagPath :: FilePath, contentPaths :: [FilePath] } deriving Show

mapUnion :: TagMap -> TagMap -> TagMap
mapUnion = unionWith union

mapUnions :: [TagMap] -> TagMap
mapUnions = unionsWith union

-- works on "a/b/" and "a/b"
readTag :: FilePath -> Tag
readTag = last . splitDirectories 

-- to be defined
readTitle :: FilePath -> Title
readTitle = takeFileName

mapPaths :: [(FilePath, Tags)] -> TagMap
mapPaths = fromListWith union

contained :: FilePath -> IO [Title]
contained dir = namesMatching (dir ++ "/*")

r :: Tag -> [Title] -> [(Tags, Title)]
r t = Prelude.map ( \ c -> (sin, c) )
  where sin = Data.Set.singleton t

collectBranches :: IO [FilePath]
collectBranches = do
  home <- getCurrentDirectory
  contained (home ++ "/" ++ rootDir)

filterContents :: (Tags -> Bool) -> [Tagged Title Tag] -> [Title]
filterContents pred = Prelude.map content . filter (pred . features)

onTitles :: (Title -> Title) -> [(Tag, [Title])] -> [(Tag, [Title])]
onTitles transform = Prelude.map (\ (t, c) -> (t, Prelude.map transform c))

fromBranch :: FilePath -> [FilePath] -> TagMap
fromBranch tagPath = mapUnions . Prelude.map tagSingleton
  where tagSingleton titlePath = Data.Map.Strict.singleton title tags
          where tags = Data.Set.singleton (readTag tagPath)
                title = readTitle titlePath

readBranches :: [Branch] -> [Tagged Title Tag]
readBranches = let mapBranch :: Branch -> TagMap
                   mapBranch (Branch tagPath titlePaths) = fromBranch tagPath titlePaths
                   readMap :: TagMap -> [Tagged Title Tag]
                   readMap = Prelude.map (uncurry Tagged) . toList
               in readMap . mapUnions . (Prelude.map mapBranch)

allTags :: [Branch] -> Set Tag
allTags = fromList . Prelude.map (readTag . tagPath)

walk :: IO [Branch]
walk = do
  paths <- collectBranches
  sequence (Prelude.map walkBranch paths)
    where walkBranch path = do
            contentPaths <- contained path
            pure (Branch path contentPaths)
          walkBranch :: FilePath -> IO Branch

-- filtering logic modeled after a nand in order to narrow strictly
filterIntersection :: Tags -> Tags -> [Tagged Title Tag] -> [Title]
filterIntersection all focused = filterContents pred
  where pred = null . intersection (difference all focused)

data Options = Options {
  intersectionOption :: [FilePath]
  }

data Query = Only Tags | All Tags

getQuery :: Options -> Query
getQuery (Options o) = Only (fromList o)

runQuery :: Query -> Tags -> [Tagged Title Tag] -> [Title]
runQuery (Only focused) all = filterIntersection all focused

select :: Options -> IO [Title]
select opt = do
  branches <- walk
  pure (runQuery (getQuery opt) (allTags branches) (readBranches branches))

optionParser :: Parser Options
optionParser = Options
               <$> some (strOption (long "intersection" <> short 'i'))

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

main = execParser optionParserInfo >>= select >>= sequence . Prelude.map putStrLn
