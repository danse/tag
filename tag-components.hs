import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Foldable
import qualified Data.Tree     as Tree
import qualified Data.List     as List
import qualified Data.Graph    as Graph
import Data.Map.Lazy (Map(..))
import Data.Set      (Set(..))
import Data.Tree     (Tree(..), Forest(..))
import Data.Graph     (Graph(..), Vertex(..))
import Data.Function ((&), on)
import Tag

subsetEdges :: [TaggedSet] -> [(Set Tagged, Tag, [Tag])]
subsetEdges tagged =
  let f :: TaggedSet -> (Set Tagged, Tag, [Tag])
      f = g (taggedSet, taggedSetTag, fmap taggedSetTag . subsets tagged)
      g (a, b, c) t = (a t, b t, c t)
  in fmap f tagged

type GetNode   = Vertex -> (Set Tagged, Tag, [Tag])
type GetVertex = Tag -> Maybe Vertex
type GetTag    = Vertex -> Tag

subGraph :: [TaggedSet] -> (Graph, GetNode, GetVertex)
subGraph = Graph.graphFromEdges . subsetEdges

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c

showVertexForest :: GetNode -> Forest Vertex -> String
showVertexForest getNode = Tree.drawForest . mapVertexes
  where mapVertexes :: Forest Vertex -> Forest Tag
        mapVertexes = fmap (fmap (snd3 . getNode))

readSubGraph = do
  g <- readInvertedGraph
  return (g & graphToTagged & subGraph)
  
showForest :: (Graph -> Forest Vertex) -> IO ()
showForest f = do
  (g', getNode, _) <- readSubGraph
  putStr (g' & f & showVertexForest getNode)

showSort :: IO ()
showSort = do
  (g', getNode, _) <- readSubGraph
  putStr (g' & Graph.topSort & fmap (snd3 . getNode) & unlines)

main :: IO ()
main = showForest Graph.components
