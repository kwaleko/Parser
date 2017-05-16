{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where
  
import                Control.Monad.Trans(MonadIO(..))
import                Control.Applicative (Alternative(..),(<|>))
import                GHC.Generics
--import qualified      Parser  --as P
  
--import            Data.Time(UTCTime)

data Article = Article 
    {
     artId       ::  Maybe Int
    ,artTitle    ::  String
    ,artBody     ::  String
    } 
    deriving (Eq,Show,Generic)
    
-- a parser of things is a function from strings to list of pair of things and strings    
newtype Parser a = P (  String -> [(a,String)])

-- file system and do not do any other monad.
newtype HandleIO a = HandleIO { runHandleIO :: IO a }
  deriving (Functor,Applicative,Monad)
 


--function that remove the constructer and return the parser result.
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp 

-- fmap applies a funtion to the parser result if the it succeeds, propagates the failure otherwise.
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\inp -> case parse p inp of
    [] -> []
    [(v,out)] -> [(f v,out)])

-- comments  
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure val = P (\inp -> [(val,inp)])
  

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b 
  pf <*> pv = P (\inp -> case parse pf inp of
    [] -> []
    [(g,out)] -> parse (fmap g pv) out)
 
   
-- make the parser to be instance of Monad. 
instance Monad Parser  where
  -- (>>= ) :: Parser a -> (a -> Parser b) -> Parser c
  p >>= f = P (\inp -> case parse p inp of
    [] -> []
    [(v,out)] -> parse (f v) out)
    
  fail _ = P (\inp -> [])
  
instance Alternative Parser where
  --empty :: Parser a
  empty = P (\inp -> [])
  
  -- <|> Parse a -> Parser a -> Parser a
  p1 <|> p2 = P (\inp -> case parse p1 inp of 
    [] ->  parse p2 inp
    [(v,out)] -> [(v,out)])
 