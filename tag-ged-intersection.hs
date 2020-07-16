import System.Directory (listDirectory)
import Tag (takePathEnd)
import System.FilePath.Glob (namesMatching)

data Options = Options {
  tagPaths :: [FilePath]
 }

data Match = Match {
  where :: FilePath,
  what  :: String
}

-- https://downloads.haskell.org/~ghc/8.6.1/docs/html/libraries/directory-1.3.3.0/System-Directory.html#v:listDirectory

tagged tags = getCurrentDirectory >>= listDirectory (curr ++ "/tags")

matching dirs = 

(tag, namesMatching (curr ++ "/tags/" ++ tag ++ "/?*")

union :: Options -> IO [Strings]
union (Options {..}) = do
  dirs <- tagged (map takePathEnd tagPaths)
  tagged (map takePathEnd tagPaths)
  
