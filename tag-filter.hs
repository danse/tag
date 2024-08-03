import Tag
import Data.Functor ((<$>))
import Options.Applicative
import Data.Hypergraph (nodes)
import Data.Map (keys, Map)
import Data.Set (Set)
import Data.Function ((&))
import Data.Foldable (length)
import Lens.Micro.Extras (view)

import qualified Data.Map as Map
import qualified Data.Set as Set

data Options = Options {
  include :: [String],
  exclude :: [String],
  minCard :: Maybe Int,
  maxCard :: Maybe Int,
  optionsOr :: Bool,
  showLines :: Bool,
  path :: Maybe String}

optionParser :: Parser Options
optionParser = Options
               <$> many (strOption (long "include" <> short 'i'))
               <*> many (strOption (long "exclude" <> short 'e'))
               <*> optional (option auto (long "min-occurrences"))
               <*> optional (option auto (long "max-occurrences"))
               <*> switch (long "combine-with-or" <> short 'o')
               <*> switch (long "show-lines" <> short 'l')
               <*> optional (strOption (long "path"))

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

select :: [Set Tag -> Bool] -> Bool -> Tagged String -> [String]
select preds o =
  let applyAll :: Traversable t => t (a -> b) -> a -> t b
      applyAll funcs arg = fmap (arg &) funcs
      con :: Traversable t => t Bool -> Bool
      con
        | o         = or
        | otherwise = and
  in keys . Map.filter (con . applyAll preds) . view nodes

makePreds :: [Tag]
          -> [Tag]
          -> Maybe Int
          -> Maybe Int
          -> [Set Tag -> Bool]
makePreds i x min' max' =
  let p1 :: Set Tag -> Bool
      p1 s
        | null i    = True
        | otherwise = (length i == length (Set.intersection (Set.fromList i) s))
      p2 = null . Set.intersection (Set.fromList x)
      p3 s = maybe True (\ card -> length s >= card) min'
      p4 s = maybe True (\ card -> length s <= card) max'
  in [p1, p2, p3, p4]

format :: Maybe String -> String -> String
format maybePath result = case maybePath of
  Just path -> "\"tags/" <> path <> "/" <> result <> "\""
  Nothing   -> result

main = do
  (Options i x min max o l path) <- execParser optionParserInfo
  graph <- readGraph
  let tags = fmap Tag
      preds = makePreds (tags i) (tags x) min max
      res = select preds o graph
  if l
    then mapM_ (putStrLn . format path) res
    else print res
