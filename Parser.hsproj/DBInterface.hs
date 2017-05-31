module                DBInterface   
                      (
                        addArticle
                       ,createUser 
                       ,dbSchema
                       ,getArticle
                      ) 
                        where
  
import                Control.Monad.Trans.Reader  (ask,ReaderT)
import                Database.HDBC.Sqlite3       (Connection)


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
        
-- add new Article
addArticle  ::    Article 
               -> ReaderT Connection IO ()
addArticle
  article = do
    conn <- ask
    Db.sqlRun sql [ Db.toSql (artId    article)
                   ,Db.toSql (artTitle article)
                   ,Db.toSql (artBody  article) 
                   ]
    where
      sql = "insert into Articles (Title, Content ,UserId) values (?,?,?)"                
                   
 -- Add user to the database
createUser      ::   User 
                  -> ReaderT Connection IO ()
createUser  
  user =  do
    conn <- ask
    Db.sqlRun sql [Db.toSql (userAccount user)
                   ,Db.toSql (userName   user)]
                   
    where
      sql = " INSERT INTO users (account, name) VALUES (?,?)"
 

{- 
getArticle   ::    Id -> ReaderT Connection IO (Maybe Article)               
getArticle id = do
    conn <- ask
    Db.sqlQueryOne sql [Db.toSql id] transform
    where 
      sql = "SELECT (id,title,content) FROM articles where id = ? "
      transform = \xs -> Article (Db.fromSqlToInt (xs !! 0)) (Db.fromSqlToString (xs !! 1)) (Db.fromSqlToString (xs !! 2)) -}
          
