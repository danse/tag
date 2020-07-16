{-# LANGUAGE RecordWildCards #-}
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, copyFile, removeFile, getCurrentDirectory)
import System.FilePath.Posix (takeDirectory, takeFileName)
import System.FilePath.Glob (namesMatching)
import System.FilePath.Manip (renameWith)
import System.Posix.Files (rename)
import Data.Monoid( (<>) )
import Control.Applicative( some )
import Control.Monad (void)
import Options.Applicative
import Tag (rootDir)

data Options = Options {
  tagged :: String,
  name :: String,
  preview :: Bool
}

withFileName newFileName full = takeDirectory full ++ "/" ++ newFileName

optionParser :: Parser Options
optionParser = Options
               <$> strOption (long "tagged" <> short 't')
               <*> strOption (long "name" <> short 'n')
               <*> switch (long "preview" <> short 'p')

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

renameAll :: Options -> IO ()
renameAll Options {..} = 
  let fileName = takeFileName tagged
      renameCopies = void . sequence . map (renameWith (withFileName name))
      put = void . sequence . map (\f-> putStrLn (name ++ " " ++ f))
  in do
    curr   <- getCurrentDirectory
    tagged <- namesMatching (curr ++ "/" ++ rootDir ++ "/?*/" ++ fileName)
    homed  <- namesMatching (curr ++ "/" ++ fileName)
    (if preview then put else renameCopies) (homed ++ tagged)

main = execParser optionParserInfo >>= renameAll
