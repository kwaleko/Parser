{-# LANGUAGE RankNTypes #-}

module                DBUtils 
                     ( 
                      -- Commit data to DB
                       cmdExecute
                      ,cmdCommit
                      -- convert sql value to types
                      ,fromSqlToInt
                      ,fromSqlToString
                      ,toSql                    
                      -- Select data from DB
                      ,selectOne
                      ,selectMany
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

fromSqlToInt      :: SqlValue -> Int
fromSqlToInt sv   = 
  fromSql sv

fromSqlToString   :: SqlValue -> String
fromSqlToString sv = 
  fromSql sv

-- Get a single field form database  
selectOne              ::     SQL 
                          -> [SqlValue] 
                          -> (SqlValue -> a) 
                          -> ReaderT Connection IO (Maybe a)
selectOne 
  sql sqlvals transform = do
  conn <- ask
  stmt <- liftIO $ prepare conn sql
  liftIO $ execute stmt sqlvals
  row <- liftIO $ fetchRow  stmt
  return $ head . (map transform) <$> row
 
-- Get Multiple value from database
selectMany             ::     SQL 
                          -> [SqlValue] 
                          -> ([SqlValue] -> a) 
                          -> ReaderT Connection IO [a]
selectMany 
  sql sqlvals transform = do
  conn <- ask
  stmt <- liftIO $ prepare conn sql
  liftIO $ execute stmt sqlvals
  rows <- liftIO $ fetchAllRows stmt
  return $ fmap transform rows
  
-- Execute statement that returns no values
cmdExecute             ::     SQL
                          -> [SqlValue]
                          -> ReaderT Connection IO ()          
cmdExecute  
  sql sqlvals = do
  conn <- ask
  stmt <- liftIO $ prepare conn sql 
  liftIO $ execute stmt sqlvals
  return ()

-- Commit a database action  
cmdCommit             ::   ReaderT Connection IO () 
                        -> ReaderT Connection IO ()
cmdCommit 
  dbaction = do
  conn <- ask
  dbaction
  liftIO $ commit conn
  return () 
  