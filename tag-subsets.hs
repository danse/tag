import qualified Data.Map.Lazy as Map
import qualified Data.Tree     as Tree
import qualified Data.List     as List
import Data.Map.Lazy (Map)
import Data.Set      (Set)
import Data.Tree     (Tree(..))
import Data.Function ((&))
import Tag

unfolding :: [TaggedSet] -> TaggedSet -> (Tag, [TaggedSet])
unfolding allSets branch@(TaggedSet t _) = (t, subsets allSets branch)

supForest :: [TaggedSet] -> Tree.Forest Tag
supForest a = Tree.unfoldForest (unfolding a) a

treeDepth :: Tree Tag -> Int
treeDepth = length . Tree.levels

toSortedForest :: Map Tag (Set Tagged) -> Tree.Forest Tag
toSortedForest = reverse
               . List.sortOn treeDepth
               . supForest
               . map (uncurry TaggedSet)
               . Map.toList

removeDuplicates :: [Tag] -> [Tree Tag] -> [Tree Tag]
removeDuplicates  _ [] = []
removeDuplicates [] (t:ts) = t : removeDuplicates (Tree.flatten t) ts
removeDuplicates known ts = removeDuplicates [] (filter filtering ts)
  where filtering :: Tree Tag -> Bool
        filtering tree = not (elem (rootLabel tree) known)

{-

duplication detection relies on sorting. A tree which comes before its
superset will not be detected as a duplicate

-}

main :: IO ()
main = do
  gra <- readInvertedGraph
  gra & toSortedForest
      & removeDuplicates []
      & Tree.drawForest
      & putStr

type SubsetTrees = [Tree Tag]

g = undefined
g :: [TaggedSet] -> SubsetTrees

h = undefined
h :: SubsetTrees -> [Tree Tag]
