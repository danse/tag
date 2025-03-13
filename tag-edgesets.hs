import Data.Hypergraph (edgeSets)
import Data.List (sortOn)
import Data.Map (Map, insertWith, toList)
import Data.Set (Set, size, toList)
import Tag (readGraph, Tag, dec)

type S = [(Set Tag, Set String)]

s :: IO S
s = edgeSets <$> readGraph

r :: S -> Map (Set Tag) Int
r = foldr (\(t, n) -> insertWith (+) t (size n)) mempty

m :: (Set Tag, Int) -> String
m (t, n) =
  show n <> ", " <> (unwords . fmap dec . Data.Set.toList $ t)

h = unlines . fmap m . sortOn snd . Data.Map.toList

main = putStrLn =<< fmap (h . r) s
