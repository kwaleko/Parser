{-# LANGUAGE RankNTypes #-}

module    DBUtils     where
  
import                Control.Monad.Trans         (liftIO)
import                Control.Monad.Trans.Reader
import                Database.HDBC               ( commit
                                                  ,execute
                                                  ,fetchRow
                                                  ,fetchAllRows
                                                  ,fromSql
                                                  ,prepare 
                                                  ,run
                                                  ,SqlValue
                                                  )                                   
import                Database.HDBC.Sqlite3       (Connection
                                                  ,connectSqlite3
                                                  )


import                Types 

fromSqlToInt :: SqlValue -> Int
fromSqlToInt sv = fromSql sv

fromSqlToString :: SqlValue -> String
fromSqlToString sv = fromSql sv

createTable ::Connection -> SQL -> IO Integer
createTable conn query = do
  r <- run conn query []
  commit conn
  return r

-- Get a single value form table  
sqlQueryOne ::     Connection 
                -> SQL 
                -> [SqlValue] 
                -> (SqlValue -> a) 
                -> IO (Maybe a)
sqlQueryOne conn sql sqlvals transform = do
  stmt <- prepare conn sql
  execute stmt sqlvals
  row <- fetchRow  stmt
  return $ head . (map transform ) <$> row
 
-- Get a single value form table 
sqlQueryAll ::    Connection 
               -> SQL 
               -> [SqlValue] 
               -> ([SqlValue] -> a) 
               -> IO [a]
sqlQueryAll conn sql sqlvals transform = do
  stmt <- prepare conn sql
  execute stmt sqlvals
  rows <- fetchAllRows stmt
  return $ fmap transform rows
  

sqlRun :: Connection 
          -> SQL
          -> [SqlValue]
          -> IO ()          
sqlRun conn sql sqlvals= do
  stmt <- prepare conn sql 
  execute stmt sqlvals
  return ()
  
  

  
  
  


  

