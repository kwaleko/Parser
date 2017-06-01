module                DBInterface   
                      (
                        addArticle
                       ,addUser
                       ,dbSchema
                      ) 
                        where
  
import                Control.Monad.Trans.Reader  (ask,ReaderT)
import                Database.HDBC.Sqlite3       (Connection)


import qualified      DBUtils as Db
import                Types



-- Get Article title from db
getArticleTitle          ::     ArticleId -> ReaderT Connection IO (Maybe String)
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
addArticle  ::    Article -> ReaderT Connection IO ()
addArticle
  article = do
    conn <- ask
    Db.sqlRun sql [ Db.toSql (artTitle article)
                   ,Db.toSql (artBody  article) 
                   ]
    where
      sql = "insert into Articles (Title, Content ,UserId) values (?,?,?)"                
                   
 -- Add user to the database
addUser      ::   User -> ReaderT Connection IO ()
addUser  
  user =  do
    conn <- ask
    Db.sqlRun sql [ Db.toSql (userAccount user)
                   ,Db.toSql (userName    user)]
                   
    where
      sql = " INSERT INTO users (account, name) VALUES (?,?)"
 

-- Get single Article by id
getArticle   ::    Id -> ReaderT Connection IO [WithId Article]               
getArticle id = do
    conn <- ask
    Db.sqlQueryAll sql [Db.toSql id] transform
    where 
      sql = "SELECT (id,title,content) FROM articles where id = ? "
      transform = \xs -> WithId (Db.fromSqlToInt (xs !! 0)) $ Article  (Db.fromSqlToString (xs !! 1)) (Db.fromSqlToString (xs !! 2)) 
      
-- get All Articles
getArticles  :: ReaderT Connection IO [WithId Article]               
getArticles  =  do
    conn <- ask
    Db.sqlQueryAll sql [] transform
    where 
      sql = "SELECT (id,title,content) FROM articles "
      transform = \xs -> WithId (Db.fromSqlToInt (xs !! 0)) $ Article  (Db.fromSqlToString (xs !! 1)) (Db.fromSqlToString (xs !! 2)) 
          
