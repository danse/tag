{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Map.Lazy as Map
import qualified Data.Tree     as Tree
import qualified Data.List     as List
import Data.Hypergraph (edges)
import Data.Map.Lazy (Map)
import Data.Set      (Set)
import Data.Tree     (Tree(..))
import Data.Function ((&))
import Lens.Micro.Extras (view)
import Tag

unfolding :: [TaggedSet] -> TaggedSet -> (String, [TaggedSet])
unfolding allSets branch@(TaggedSet (Tag t) _) = (t, subsets allSets branch)

supForest :: [TaggedSet] -> Tree.Forest String
supForest a = Tree.unfoldForest (unfolding a) a

treeDepth :: Tree a -> Int
treeDepth = length . Tree.levels

toSortedForest :: Map Tag (Set String) -> Tree.Forest String
toSortedForest = reverse
               . List.sortOn treeDepth
               . supForest
               . map (uncurry TaggedSet)
               . Map.toList

removeDuplicates :: forall a . Eq a => [a] -> [Tree a] -> [Tree a]
removeDuplicates  _ [] = []
removeDuplicates [] (t:ts) = t : removeDuplicates (Tree.flatten t) ts
removeDuplicates known ts = removeDuplicates [] (filter filtering ts)
  where filtering :: Tree a -> Bool
        filtering tree = not (elem (rootLabel tree) known)

{-

duplication detection relies on sorting. A tree which comes before its
superset will not be detected as a duplicate

-}

main :: IO ()
main = do
  gra <- readGraph
  gra & view edges
      & toSortedForest
      & removeDuplicates []
      & Tree.drawForest
      & putStr

type SubsetTrees = [Tree Tag]

g = undefined
g :: [TaggedSet] -> SubsetTrees

h = undefined
h :: SubsetTrees -> [Tree Tag]
