module Database where
  
import                   Control.Monad(join)
import                   Control.Monad.Reader
import                   System.IO(IOMode(..),Handle)

import qualified         Types    as T
import qualified         HandleIO as H
import qualified         Parser   as P

-- Get Articles from File database.
getArticlesFromDB :: FilePath -> T.HandleIO (Maybe [T.Article])
getArticlesFromDB path = do
  handle <- H.openF path ReadMode
  str <- H.readF handle
  let result = T.parse ((P.chunksToArticles . P.chunks)  <$>  (P.break ';')) str
  return $ fst . head $ result
--  H.closeF handle
--  return val
  
-- get one article from database 
getArticleById :: FilePath -> Int -> T.HandleIO (Maybe T.Article)
getArticleById path id = do
  handle <- H.openF path ReadMode
  str <- H.readF handle
  let result = T.parse ((P.articlesLookup . P.chunksToArticles . P.chunks)  <$>  (P.break ';')) str
  let lookupList = fst . head $ result
  return $  join $ lookup (Just id) <$> lookupList
  

getArticlesFromDB' :: ReaderT FilePath T.HandleIO (Maybe [T.Article])
getArticlesFromDB' = ReaderT $ \file -> do
  handle <- H.openF file ReadMode
  str <- H.readF handle
  let result = T.parse ((P.chunksToArticles . P.chunks)  <$>  (P.break ';')) str
  return $ fst . head $ result

