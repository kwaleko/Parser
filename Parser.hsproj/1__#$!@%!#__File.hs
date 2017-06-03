module File (listDirectory,notDot) where
  
import Control.Monad(liftM)
import System.Directory(getDirectoryContents,doesDirectoryExist)
  
notDot :: String -> Bool
notDot val = val /= "." && val /= ".."

listDirectory :: FilePath -> IO [String]
listDirectory p = fmap (filter notDot) (getDirectoryContents p)

  