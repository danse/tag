--import System.Environment (getArgs)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on, (&))
import Control.Monad (join)
import Data.List (sort)
import Tag

data Duplicates a = Duplicates { unDuplicates :: [a] }

-- | Calculate set sizes
--
-- >>> setSizes  (Map.fromList [("tagged", Set.fromList ["t1", "t2"]), ("another", Set.fromList ["t2", "t3"])])
-- fromList [("t1",1),("t2",2),("t3",1)]
setSizes :: Map String (Set String) -> Map String Int
setSizes =
  let countTag :: String -> Map String Int -> Map String Int
      countTag t = Map.insertWith (+) t 1
      collectTags :: Map String (Set String) -> Duplicates String
      collectTags = Duplicates . join . fmap Set.toList . Map.elems
      countTags :: Duplicates String -> Map String Int
      countTags = foldr countTag Map.empty . unDuplicates
  in countTags . collectTags

data SetSize = SetSize { unSetSize :: (String, Int) }
instance Eq SetSize where
  (==) = on (==) (snd . unSetSize)
instance Ord SetSize where
  (<=) = on (<=) (snd . unSetSize)

-- | Select a tag for every tagged item. We assume that every @Set@ value in the
-- second argument of @selectSmallest@ in not empty, otherwise this
-- function will not be able to pick the smallest and it will throw an
-- exception
--
-- >>> selectSmallest (Map.fromList [("t1", 1), ("t2", 2)]) (Map.fromList [("tagged", Set.fromList ["t1", "t2"])])
-- fromList [("tagged","t1")]
selectSmallest :: Map String Int -> Map String (Set String) -> Map String String
selectSmallest sizes =
  let 
    addSize :: String -> (String, Maybe Int)
    addSize t = (t, Map.lookup t sizes)
    -- | The whole graph was already traversed to compute the sizes so
    -- we expect all the maybes to host @Just@ values
    onlyMatches :: [(a, Maybe b)] -> [(a, b)]
    onlyMatches ((_, Nothing):rest) = onlyMatches rest
    onlyMatches ((t, Just i) :rest) = (t, i) : onlyMatches rest
    onlyMatches [] = []
    toSizes :: Set String -> [SetSize]
    toSizes = map SetSize
        . onlyMatches
        . map addSize
        . Set.toList
    smallestTag :: [SetSize] -> String
    smallestTag = fst . unSetSize . head . sort
  in fmap (smallestTag . toSizes)

format :: Map String String -> String
format =
  let formatOne :: (String, String) -> String
      formatOne (file, tag) = tag++"/"++ file
  in unlines . sort . fmap formatOne . Map.toList

main :: IO ()
main = do
  branches <- walk
  let graph = parseGraph branches :: Map String (Set String)
      sizes = setSizes graph :: Map String Int
    in putStr (selectSmallest sizes graph & format)
