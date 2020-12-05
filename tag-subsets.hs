import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Foldable
import qualified Data.Tree     as Tree
import qualified Data.List     as List
import Data.Map.Lazy (Map(..))
import Data.Set      (Set(..))
import Data.Tree     (Tree(..))
import Data.Function ((&), on)
import Tag

data TaggedSet = TaggedSet { taggesSetTag :: Tag, unTaggedSet :: Set Tagged }

subsets :: [TaggedSet] -> TaggedSet -> [TaggedSet]
subsets candidates (TaggedSet t1 s1) =
  let filtering :: TaggedSet -> Bool
      filtering (TaggedSet t2 s2) = s2 `Set.isProperSubsetOf` s1
  in List.filter filtering candidates

unfolding :: [TaggedSet] -> TaggedSet -> (Tag, [TaggedSet])
unfolding allSets branch@(TaggedSet t s) = (t, subsets allSets branch)

supForest :: [TaggedSet] -> Tree.Forest Tag
supForest a = Tree.unfoldForest (unfolding a) a

treeDepth :: Tree Tag -> Int
treeDepth = length . Tree.levels

-- | Try reading a tag file structure from the current directory or
-- show an error
readInvertedGraph :: IO (Map Tag (Set Tagged))
readInvertedGraph = fmap (invertGraph . parseGraph) walk

main :: IO ()
main = do
  g <- readInvertedGraph
  g & Map.toList
    & map (uncurry TaggedSet)
    & supForest
    & List.sortOn treeDepth
    & reverse
    & Tree.drawForest
    & putStr
