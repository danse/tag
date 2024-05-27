import Data.Hypergraph (edges)
import Data.Map.Lazy (Map(..))
import Data.Set      (Set(..))
import Data.Tree     (Tree(..), Forest(..))
import Data.Graph     (Graph(..), Vertex(..))
import Data.Function ((&), on)
import Lens.Micro.Extras (view)
import Tag

import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Foldable
import qualified Data.Tree     as Tree
import qualified Data.List     as List
import qualified Data.Graph    as Graph

subsetEdges :: [TaggedSet] -> [(Set String, Tag, [Tag])]
subsetEdges tagged =
  let f :: TaggedSet -> (Set String, Tag, [Tag])
      f = g (taggedSet, taggedSetTag, fmap taggedSetTag . subsets tagged)
      g (a, b, c) t = (a t, b t, c t)
  in fmap f tagged

type GetNode   = Vertex -> (Set String, Tag, [Tag])
type GetVertex = Tag -> Maybe Vertex
type GetTag    = Vertex -> Tag

subGraph :: [TaggedSet] -> (Graph, GetNode, GetVertex)
subGraph = Graph.graphFromEdges . subsetEdges

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c
tagString = (\(Tag s) -> s) . snd3

showVertexForest :: GetNode -> Forest Vertex -> String
showVertexForest getNode = Tree.drawForest . mapVertexes
  where mapVertexes :: Forest Vertex -> Forest String
        mapVertexes = fmap (fmap (tagString . getNode))

readSubGraph = subGraph . graphToTagged . view edges <$> readGraph
  
showForest :: (Graph -> Forest Vertex) -> GetNode -> Graph -> String
showForest f getNode g' = g' & f & showVertexForest getNode

showSort :: GetNode -> Graph -> String
showSort getNode g' = g' & Graph.topSort & fmap (tagString . getNode) & unlines

readPut :: (GetNode -> Graph -> String) -> IO ()
readPut f = do
  (g', getNode, _) <- readSubGraph
  putStr $ f getNode g'

main :: IO ()
main = readPut (showForest Graph.components)
