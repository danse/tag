--import System.Environment (getArgs)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on, (&))
import Control.Monad (join)
import Data.List (sort)
import Lens.Micro.Extras (view)
import Data.Hypergraph
import Tag

data Duplicates a = Duplicates { unDuplicates :: [a] }

-- | Calculate set sizes
--
-- >>> setSizes $ fromLists [("tagged", ["t1", "t2"]), ("another", ["t2", "t3"])])
-- fromList [("t1",1),("t2",2),("t3",1)]
setSizes :: Tagged String -> Map Tag Int
setSizes = fmap Set.size . view edges

data SetSize = SetSize { unSetSize :: (Tag, Int) }
instance Eq SetSize where
  (==) = on (==) (snd . unSetSize)
instance Ord SetSize where
  (<=) = on (<=) (snd . unSetSize)

-- | Select a tag for every tagged item. We assume that every @Set@ value in the
-- second argument of @selectSmallest@ in not empty, otherwise this
-- function will not be able to pick the smallest and it will throw an
-- exception
--
-- >>> selectSmallest (Map.fromList [("t1", 1), ("t2", 2)]) (fromLists [("tagged", ["t1", "t2"])])
-- fromList [("tagged","t1")]
selectSmallest :: Map Tag Int -> Tagged String -> Map String Tag
selectSmallest sizes =
  let 
    addSize :: Tag -> (Tag, Maybe Int)
    addSize t = (t, Map.lookup t sizes)
    -- | The whole graph was already traversed to compute the sizes so
    -- we expect all the maybes to host @Just@ values
    onlyMatches :: [(a, Maybe b)] -> [(a, b)]
    onlyMatches ((_, Nothing):rest) = onlyMatches rest
    onlyMatches ((t, Just i) :rest) = (t, i) : onlyMatches rest
    onlyMatches [] = []
    toSizes :: Set Tag -> [SetSize]
    toSizes = map SetSize
        . onlyMatches
        . map addSize
        . Set.toList
    smallestTag :: [SetSize] -> Tag
    smallestTag = fst . unSetSize . head . sort
  in fmap (smallestTag . toSizes) . view nodes

format :: Map String Tag -> String
format =
  let formatOne :: (String, Tag) -> String
      formatOne (file, Tag tag) = tag++"/"++ file
  in unlines . sort . fmap formatOne . Map.toList

main :: IO ()
main = do
  graph <- readGraph
  let sizes = setSizes graph :: Map Tag Int
  putStr (selectSmallest sizes graph & format)
