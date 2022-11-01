{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, copyFile, removeFile, getCurrentDirectory)
import System.FilePath.Posix (takeFileName, splitPath)
import System.Posix.Files (createLink)
import Data.Monoid( (<>) )
import Control.Applicative( some )
import Options.Applicative
import Tag
import Control.Exception (try, SomeException)
import Control.Monad (join)

type File   = String
type Report = String

data Feature = Feature { tag::Tag, tagged::File }

link file dir = do
  createDirectoryIfMissing True dir
  res <- try $ createLink file (dir ++ "/" ++ (takeFileName file)) :: IO (Either SomeException ())
  case res of
    Right _ -> pure $ "file " <> file <> " linked in " <> dir
    Left e -> pure $ show e <> " in " <> dir

store :: Bool -> Feature -> IO Report
store dry (Feature (Tag tag) tagged) = do
  curr <- getCurrentDirectory
  if dry
    then pure (tagged ++" store "++ tag)
    else link tagged (curr ++ "/" ++ rootDir ++ "/" ++ tag ++ "/")

product :: [a] -> [b] -> [(a,b)]
product a = (map (,) a <*>)

multiple :: Bool -> [Tag] -> [File] -> [IO Report]
multiple dry tags = map (store dry . uncurry Feature) . Main.product tags

doConsume :: Bool -> [String] -> IO String
doConsume dry files = if dry
                    then pure ("consuming "++ join files)
                    else do
                        sequence (map removeFile files)
                        pure ("removed "++ join files)

data Options = Options {
  consume :: Bool,
  dry :: Bool,
  tagPaths :: [FilePath],
  files :: [File]
  }

optionParser :: Parser Options
optionParser = Options
               <$> switch (long "consume" <> short 'c')
               <*> switch (long "dry" <> short 'd')
               <*> some (strOption (long "tag" <> short 't')) -- you can use this option with an existing tag to benefit from prefix completion
               <*> some (argument str (metavar "<file1> <file2> ..."))

exe :: Options -> IO ()
exe Options {..} = 
  let tags = map (Tag . takePathEnd) tagPaths
  in do
    reports <- sequence $ multiple dry tags files
    if consume
      then doConsume dry files
      else pure "preserving files"
    putStrLn (unlines reports)

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

main = execParser optionParserInfo >>= exe
