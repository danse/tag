import Tag
import Data.Functor ((<$>))
import Options.Applicative
import Data.Map (keys, Map)
import Data.Set (Set)
import Data.Function ((&))
import Data.Foldable (length)

import qualified Data.Map as Map
import qualified Data.Set as Set

data Options = Options {
  include :: [String],
  exclude :: [String],
  minCard :: Maybe Int,
  maxCard :: Maybe Int,
  optionsOr :: Bool,
  showLines :: Bool
  }

optionParser :: Parser Options
optionParser = Options
               <$> many (strOption (long "include" <> short 'i'))
               <*> many (strOption (long "exclude" <> short 'e'))
               <*> optional (option auto (long "min-cardinality"))
               <*> optional (option auto (long "max-cardinality"))
               <*> switch (long "combine-with-or" <> short 'o')
               <*> switch (long "show-lines" <> short 'l')

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

select :: [Set String -> Bool] -> Bool -> [Branch] -> Map Tagged (Set Tag)
select preds o =
  let applyAll :: Traversable t => t (a -> b) -> a -> t b
      applyAll funcs arg = fmap (arg &) funcs
      con :: Traversable t => t Bool -> Bool
      con
        | o         = or
        | otherwise = and
  in Map.filter (con . applyAll preds) . parseGraph

makePreds :: [String]
          -> [String]
          -> Maybe Int
          -> Maybe Int
          -> [Set String -> Bool]
makePreds i x min' max' =
  let p1 :: Set String -> Bool
      p1 s
        | null i    = True
        | otherwise = (length i == length (Set.intersection (Set.fromList i) s))
      p2 = null . Set.intersection (Set.fromList x)
      p3 s = maybe True (\ card -> length s >= card) min'
      p4 s = maybe True (\ card -> length s <= card) max'
  in [p1, p2, p3, p4]

main = do
  (Options i x min max o l) <- execParser optionParserInfo
  branches <- walk
  let preds = makePreds i x min max
      res = select preds o branches
  if l
    then putStr (unlines (keys (res)))
    else print res
