{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Server where


import              Control.Monad.Trans(liftIO)
import              Data.Aeson(FromJSON,ToJSON)
import              Servant
import              System.IO (hPutStrLn,stderr)
import              Network.Wai
import              Network.Wai.Handler.Warp


import qualified    Types     as T
import qualified    Parser    as P 
import qualified    HandleIO  as H
--import qualified    Database  as Db

-- * api

type ItemApi =
       "Articles" :> Get '[JSON] (Either String [T.Article]) 
  :<|> "Articles" :> Capture "itemId" String :> Get '[JSON] (Either String  T.Article)

itemApi :: Proxy ItemApi
itemApi =  Proxy

-- * app
main :: IO ()
main = Server.run 

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $ -- to replace hPutStrLn with handle IO 
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
       listArticles 
  :<|> getArticle 

handleIOtoHandler :: T.HandleIO a -> Handler a
handleIOtoHandler (T.HandleIO action) =liftIO action


listArticles :: Handler (Either String [T.Article])
listArticles =   handleIOtoHandler $ Db.getArticlesFromDB path

--listArticles' :: Handler (Maybe [T.Article])
--listArticles' =


getArticle :: String ->  Handler (Either String T.Article)
getArticle  = \ case 
  id -> handleIOtoHandler $ Db.getArticleById path id
 -- _  -> throwE err404
  
path :: FilePath
path = "/Users/lambda/dev/Parser/Parser.hsproj/Db/database.txt"

instance ToJSON T.Article
instance FromJSON T.Article

