{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where
  
import            Control.Monad.Trans(MonadIO(..))
import            GHC.Generics
  
--import            Data.Time(UTCTime)

data Article = Article 
    {
     artId       ::  Maybe Int
    ,artTitle    ::  String
    ,artBody     ::  String
    } deriving (Eq,Show,Generic)
    
-- a parser of things is a function from strings to list of pair of things and strings    
newtype Parser a = P (String -> [(a,String)])

-- file system and do not do any other monad.
newtype HandleIO a = HandleIO { runHandleIO :: IO a }
  deriving 
    (Functor,Applicative,Monad)