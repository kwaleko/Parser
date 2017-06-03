module HandleIO
      (
        openF
       ,readF
       ,writeF
       ,closeF
      ) where

import                Control.Monad.Trans(liftIO)        
import                System.IO(Handle, IOMode(..),withFile)
import qualified      System.IO as S

import                Types (HandleIO(..))


openF :: FilePath -> IOMode -> HandleIO Handle
openF path mode = HandleIO (S.openFile path mode)

readF :: Handle -> HandleIO String
readF handle = HandleIO (S.hGetContents handle)

writeF :: Handle -> String -> HandleIO ()
writeF handle message = HandleIO (S.hPutStr handle message)

closeF :: Handle -> HandleIO ()
closeF  handle = HandleIO (S.hClose handle)

--withF :: FilePath -> IOMode -> (Handle -> IO r ) -> HandleIO r
--withF  path mode action = liftIO $ withFile path mode action