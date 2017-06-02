{-# LANGUAGE RankNTypes #-}

module                DBUtils 
                     (
                       fromSqlToInt
                      ,fromSqlToString
                      ,sqlCommit
                      ,sqlQueryOne
                      ,sqlQueryAll
                      ,sqlRun
                      ,toSql                    
                     ) 
                      where
  
import                Control.Monad.Trans         (liftIO)
import                Control.Monad.Trans.Reader  (ask,ReaderT)
import                Database.HDBC.Sqlite3       (Connection,connectSqlite3)
import                Database.HDBC               ( 
                                                   commit
                                                  ,execute
                                                  ,fetchRow
                                                  ,fetchAllRows
                                                  ,fromSql
                                                  ,prepare 
                                                  ,run
                                                  ,SqlValue
                                                  ,toSql
                                                  )                                   

                                                   
                                                 
import                Types 

--fromAnyToSql    :: a -> SqlValue 
--fromAnyToSql val   = toSql val

connectDB = connectSqlite3

fromSqlToInt    :: SqlValue -> Int
fromSqlToInt sv = 
  fromSql sv

fromSqlToString   :: SqlValue -> String
fromSqlToString sv = 
  fromSql sv

-- Get a single value form database  
sqlQueryOne             ::   SQL 
                          -> [SqlValue] 
                          -> (SqlValue -> a) 
                          -> ReaderT Connection IO (Maybe a)
sqlQueryOne 
  sql sqlvals transform = do
  conn <- ask
  stmt <- liftIO $ prepare conn sql
  liftIO $ execute stmt sqlvals
  row <- liftIO $ fetchRow  stmt
  return $ head . (map transform ) <$> row
 
-- Get list of values form database 
sqlQueryAll             ::   SQL 
                          -> [SqlValue] 
                          -> ([SqlValue] -> a) 
                          -> ReaderT Connection IO [a]
sqlQueryAll 
  sql sqlvals transform = do
  conn <- ask
  stmt <- liftIO $ prepare conn sql
  liftIO $ execute stmt sqlvals
  rows <- liftIO $ fetchAllRows stmt
  return $ fmap transform rows
  
-- Execute statement that returns no values
sqlRun        ::   SQL
                -> [SqlValue]
                -> ReaderT Connection IO ()          
sqlRun  
  sql sqlvals = do
  conn <- ask
  stmt <- liftIO $ prepare conn sql 
  liftIO $ execute stmt sqlvals
  return ()

-- Commit a database action  
sqlCommit          ::   ReaderT Connection IO () 
                     -> ReaderT Connection IO ()
sqlCommit dbaction = do
  conn <- ask
  dbaction
  liftIO $ commit conn
  return () 
  