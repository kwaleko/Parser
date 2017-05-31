module                DBInterface   
                      (
                       dbSchema
                       ,getArticleTitle
                      ) where
  
import                Control.Monad.Trans.Reader  (ask,ReaderT)
import                Database.HDBC.Sqlite3       (
                                                   connectSqlite3
                                                  ,Connection
                                                  )


import qualified      DBUtils as Db
import                Types



-- Get Article title from db
getArticleTitle          ::     ArticleId 
                             -> ReaderT Connection IO (Maybe String)
getArticleTitle articleid = do
  conn <- ask 
  Db.sqlQueryOne sql [] Db.fromSqlToString 
    where
      sql = unlines [
         "SELECT articles.title FROM articles"
        ," WHERE articles.id = ? "
        ]
        
-- create table Articles
dbSchema  :: ReaderT Connection IO ()
dbSchema  = do
  conn <- ask
  Db.sqlRun sql []
    where
      sql = unlines [
           "CREATE TABLE IF NOT EXISTS articles (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,date DEFAULT CURRENT_DATE ,title VARCHAR(80),content TEXT,userid INTEGER)"
          ,"CREATE TABLE IF NOT EXISTS users (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,username VARCHAR (20),name VARCHAR(20))"
        ]
        
-- Add user to the database
createUser      :: User -> ReaderT Connection IO Int
createUser  
  user =  do
    conn <- ask
    let sqlvalues = [
                HDBC
    ]
    Db.sqlRun sql 
        
{- main :: IO ()
main = do
  conn <- connectSqlite3 "db.txt"
  Db.sqlCommit   conn  (dbSchema conn ) -}
