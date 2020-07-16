module Tag where

import System.FilePath.Posix (splitPath)
import System.Directory (getCurrentDirectory)

type Tag = String

-- this currently fails on paths ending with "/"
takePathEnd :: FilePath -> FilePath
takePathEnd = head . dropWhile null . reverse . splitPath

-- it's ridicolous to create a module only for this which is the first
-- shared resource i found at the moment and it's probably destined to
-- disappear. I did because i want more development and i think that
-- it will go through easier cohesion. There are several functions
-- that i want to turn into commands too. This could go to Prototypes
-- though
rootDir = "tags"

-- an easy alternative is to get roots from the home with
-- `System.Directory.getHomeDirectory` as i used to do before in the
-- filter
localTagsDir :: IO FilePath
localTagsDir = do
  curr <- getCurrentDirectory
  pure (curr ++ "/" ++ rootDir)

checkRootDir :: Maybe FilePath -> IO FilePath
checkRootDir maybePath = do
  local <- localTagsDir
  pure (maybe local addSlashIfMissing maybePath)
    where addSlashIfMissing = id
  
