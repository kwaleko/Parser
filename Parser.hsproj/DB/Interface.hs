module                DB.Interface   
                      (
                        addArticle
                       ,addUser
                       ,dbSchema
                       ,getArticle
                       ,getArticles
                       ,getUserName
                      ) 
                        where
  
import                Control.Monad.Trans.Reader  (ask,ReaderT)
import                Database.HDBC.Sqlite3       (Connection)


import qualified      DB.Utils as Db
import                Types




-- create tables 
dbSchema  :: ReaderT Connection IO ()
dbSchema  = do
  conn <- ask
  Db.cmdExecute sql []
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
    Db.cmdExecute sql [ Db.toSql (artTitle article)
                   ,Db.toSql (artBody  article) 
                   ]
    where
      sql = "insert into Articles (Title, Content ,UserId) values (?,?,?)"                
                   
 -- Add user to the database
addUser      ::   User -> ReaderT Connection IO ()
addUser  
  user =  do
    conn <- ask
    Db.cmdExecute sql [ Db.toSql (userAccount user)
                       ,Db.toSql (userName    user)]                   
    where
      sql = " INSERT INTO users (account, name) VALUES (?,?)"
      
getArticles :: QueryBy -> ReaderT Connection IO [WithId Article]
getArticles (ById id) = do
    conn <- ask
    Db.selectMany sql [Db.toSql id] transform
    where 
      sql = "SELECT (id,title,content) FROM articles where id = ? "
      transform = \xs -> WithId (Db.fromSqlToInt (xs !! 0)) $ Article  (Db.fromSqlToString (xs !! 1)) (Db.fromSqlToString (xs !! 2))
getArticles' (ByUser )ll = undefined 2
  
{-  
-- Get single Article by id
getArticle   ::    Id -> ReaderT Connection IO [WithId Article]               
getArticle id = do
    conn <- ask
    Db.selectMany sql [Db.toSql id] transform
    where 
      sql = "SELECT (id,title,content) FROM articles where id = ? "
      transform = \xs -> WithId (Db.fromSqlToInt (xs !! 0)) $ Article  (Db.fromSqlToString (xs !! 1)) (Db.fromSqlToString (xs  2)) -}
      
-- get All Articles
getArticles'  :: ReaderT Connection IO [WithId Article]               
getArticles'  =  doconn <- ask
    Db.selectMany sql [] transform
    where 
      sql = "SELECT (id,title,content) FROM articles "
      transform = \xs -> WithId (Db.fromSqlToInt (xs !! 0)) $ Article  (Db.fromSqlToString (xs !! 1)) (Db.fromSqlToString (xs !! 2)) 
    
      
-- Get user Name by id from table Users
getUserName :: Id -> ReaderT Connection IO (Maybe String)
getUserName id = do
  conn <- ask
  Db.selectOne sql [Db.toSql id] transform
  where
    sql       = "SELECT name FROM users WEHRE id = ? "
    transform = \sqlValue -> Db.fromSqlToString sqlValue
    
{- - Get Article by Date
getArticlesByDate ::   Month 
                    -> Year 
                    -> ReaderT Connection IO [Article]
getArticlesByDate 
  month year = do
     conn <- ask
     Db.selectMany sql [] -}
