{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module                Types   where

import                Control.Applicative (Alternative(..),(<|>)) 
import                Control.Monad.Trans(MonadIO(..))
import                Control.Monad.Trans.Reader
import                Database.HDBC (IConnection)
import                GHC.Generics
--import qualified      Parser  --as P
  
--import            Data.Time(UTCTime)

type SQL = String

type Month = Int 

type Year = Int 

type Id = Int 

data QueryBy =  ByDate String
              | ByUser Int
              | ById   Id 
              | All

type ArticleId = Int

type DBConn a = forall conn. (IConnection conn) => ReaderT conn IO a

data Article = Article 
    {
     artTitle    ::  String
    ,artBody     ::  String
    --,artUser     ::  User
    } 
    deriving (Eq,Show,Generic)
    
data User = User         
        {
         userName    :: String
        ,userAccount :: String  
        } deriving(Eq,Show)
    

newtype Parser a = Parser { 
    parse :: String -> Either String (a,String) 
  }
  
-- add id to a given , for selecting data from database.
data WithId a = WithId { id :: Id , model :: a}
    
-- file system and do not do any other monad.
newtype HandleIO a = 
    HandleIO { runHandleIO :: IO a }
    deriving 
      (Functor,
       Applicative,
       Monad)

instance Functor Parser where
  fmap f p = Parser (\inp -> case parse p inp of
    Left error -> Left error 
    Right (v,out) -> Right (f v, out))

instance Applicative Parser where
  pure val = Parser (\inp -> Right (val,inp))
  
  pf <*> pv = Parser (\inp -> case parse pf inp of
    Left error -> Left error
    Right (g,out) -> parse (fmap g pv) out )
  
   
  
instance Monad Parser where
  p >>= f = Parser (\inp -> case parse p inp of 
    Left error -> Left error
    Right (v,out) -> parse (f v) out) 
    
  fail error = Parser (\inp -> Left error)
  

instance Alternative Parser where
  --empty :: Parser a
  empty = Parser (\inp -> Left "error")
  
  -- <|> Parse a -> Parser a -> Parser a
  p1 <|> p2 = Parser (\inp -> case parse p1 inp of 
    Left _ ->  parse p2 inp
    Right (v,out) -> Right (v,out))
    

 