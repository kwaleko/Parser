module                DB.Articles(by)  where

import                Control.Monad.Trans.Reader  (ask,ReaderT)
import                Database.HDBC.Sqlite3       (Connection)
 
import                DB.Utils as Db 
import                Types

--transform  :: [SqlValue] 
transform 
  xs =  
  WithId (Db.fromSqlToInt (xs !! 0)) $ 
  Article  (Db.fromSqlToString (xs !! 1)) 
           (Db.fromSqlToString (xs !! 2))
           
-- query 
by          ::    QueryBy 
               -> ReaderT Connection IO [WithId Article]
by (ById id) = do
  conn <- ask
  Db.selectMany sql [Db.toSql id] transform
  where
    sql = "SELECT (id,title,content) FROM articles where id = ? "
by All = do
  conn <- ask
  Db.selectMany sql []  transform
  where
    sql = "SELECT (id,title,content) FROM articles"
    
by (ByUser id) = undefined