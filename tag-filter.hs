import Tag
import Data.Functor ((<$>))
import Options.Applicative
import Data.Map (keys)

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

main = do
  (Options i x min max o l) <- execParser optionParserInfo
  branches <- walk
  let res = (select i x min max o branches)
  if l
    then putStr (unlines (keys (res)))
    else print res
