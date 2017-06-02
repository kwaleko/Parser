module Database where
  
import                   Control.Monad(join)
import                   Control.Monad.Reader
import                   System.IO(IOMode(..),Handle)

import qualified         Types    as T
import qualified         HandleIO as H
import qualified         Parser   as P



-- Get Articles from File database.
getArticlesFromDB :: FilePath -> T.HandleIO (Either String [T.Article])
getArticlesFromDB path = do
  handle <- H.openF path ReadMode
  str <- H.readF handle
  let result = T.parse P.articles str
  case result of
    Right (out,_) -> return $ Right out
    Left err -> return $ Left err

  
-- get one article from database 
getArticleById :: FilePath -> String -> T.HandleIO (Either String T.Article)
getArticleById path id = do
  handle <- H.openF path ReadMode
  str <- H.readF handle
  let result = T.parse (P.article id) str
  case result of
    Right (out,_) -> return $ Right out
    Left err -> return $ Left err
  

{- getArticlesFromDB' :: ReaderT FilePath T.HandleIO (Either String [T.Article])
getArticlesFromDB' = ReaderT $ \file -> do
  handle <- H.openF file ReadMode
  str <- H.readF handle
  let result = T.parse P.parseArticles str
  return $ fst . head $ result
  -}

