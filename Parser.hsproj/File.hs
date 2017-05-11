module File (countEntries) where
  
import System.Directory(getDirectoryContents,doesDirectoryExist)
import Control.Monad(forM,liftM)
import System.FilePath ((</>))
import Control.Monad.Trans(liftIO)

notDot :: String -> Bool
notDot val = val /="." && val /=".."

listDirectory :: FilePath -> IO [String]
listDirectory path = fmap (filter notDot) (getDirectoryContents path) 

-- countries using do notation
countEntriesDo :: FilePath -> IO [(FilePath,Int)]
countEntriesDo path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
    let newName= path </> name
    isDir <- doesDirectoryExist newName
    if isDir 
      then countEntriesDo newName
      else return []    
  return $ (path, length contents) : concat rest

  
countEntries :: FilePath -> IO [(FilePath,Int)]
countEntries path= listDirectory  path >>= \contents->(
  forM contents (\name -> 
    let newName = path </> name in (
      doesDirectoryExist newName >>= \isDir ->(
        if isDir
          then countEntries newName
          else return []
      )
     )
  ) >>= \rest -> return $ (path, length contents) : concat rest     
 )
  
-- a function that reads the a given database file.
readDb :: FilePath -> IO String
readDb = undefined

   