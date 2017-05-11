{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import              Control.Monad.Trans.Except
import              Control.Monad.Trans(liftIO)
import              Data.Aeson
import              Servant
import              System.IO
import              Network.Wai
import              Network.Wai.Handler.Warp


import qualified    Types   as T
import qualified    Parser  as P 

-- * api

type ItemApi =
  "item" :> Get '[JSON] [T.Post] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] T.Post

itemApi :: Proxy ItemApi
itemApi = Proxy



-- * app
main :: IO ()
main = Server.run 

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  getPosts :<|>
  getItemById

getItems :: Handler [T.Post]
getItems = return [exampleItem]

postList :: IO [T.Post]
postList = do
  inpStr <- readFile "/Users/lambda/dev/Parser/Parser.hsproj/Db/database.txt"
  let [(result,_)] = P.parse (P.beautify <$> P.parsePosts) inpStr
  return result

getPosts :: Handler [T.Post]
getPosts = liftIO postList :: Handler [T.Post]

getItemById :: Integer -> Handler T.Post
getItemById = \ case
  0 -> return exampleItem
  _ -> throwE err404

exampleItem :: T.Post
exampleItem = T.Post "" "example item"


-- * item

instance ToJSON T.Post
instance FromJSON T.Post

