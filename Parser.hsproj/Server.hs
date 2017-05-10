{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import              Control.Monad.Trans.Except
import              Data.Aeson
import              Servant
import              System.IO
import              Network.Wai
import              Network.Wai.Handler.Warp


import qualified    Types  as T
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
  getItems :<|>
  getItemById

getItems :: Handler [T.Post]
getItems = return [exampleItem]

--getPosts :: Handler (IO String)
--getPosts = return $ getLine

getItemById :: Integer -> Handler T.Post
getItemById = \ case
  0 -> return exampleItem
  _ -> throwE err404

exampleItem :: T.Post
exampleItem = T.Post "" "example item"


-- * item

instance ToJSON T.Post
instance FromJSON T.Post

