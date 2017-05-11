module HandleIO
      (
        openF
       ,readF
       ,closeF
      ) where
        

import              System.IO(Handle, IOMode(..))
import qualified    System.IO as S

import          Types (HandleIO(..))

openF :: FilePath -> IOMode -> HandleIO Handle
openF path mode = HandleIO (S.openFile path mode)

readF :: Handle -> HandleIO String
readF handle = HandleIO (S.hGetContents handle)

closeF :: Handle -> HandleIO ()
closeF  handle = HandleIO (S.hClose handle)