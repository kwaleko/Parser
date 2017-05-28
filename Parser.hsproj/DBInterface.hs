module                DBInterface   where
  
import                Control.Monad.Trans 
import                Database.HDBC.Sqlite3 (
                                              connectSqlite3
                                             ,Connection
                                             )


import qualified      DBUtils as Db
import                Types



-- Get Article title from db
getArticleTitle :: Connection ->   ArticleId -> IO (Maybe String)
getArticleTitle conn  articleid = do
--  conn <- connectSqlite3 "db.txt" 
  Db.sqlQueryOne conn sql [] Db.fromSqlToString 
    where
      sql = unlines [
         "SELECT articles.title FROM articles"
        ," WHERE articles.id = ? "
        ]
        
-- create table Articles
dbSchema ::Connection -> IO ()
dbSchema  conn = do
--  conn <- connectSqlite3 "db.txt"
  Db.sqlRun conn sql []
    where
      sql = unlines [
          "CREATE TABLE articles (id INTEGER NOT NULL,title VARCHAR(80),content VARCHAR(1000))"
        ]
        
main :: IO ()
main = do
  conn <- connectSqlite3 "db.txt"
  Db.sqlCommit   conn  (dbSchema conn )
