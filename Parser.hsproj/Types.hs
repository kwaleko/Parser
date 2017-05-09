{-# LANGUAGE DeriveGeneric #-}
module Types where
  
import           GHC.Generics
  
data Post = Post String String 
  deriving Show 
  
-- Type of tags available in the text file.  
data TextType 
  = Title
  | Body
  deriving Show
  

{- 
        a parser for things
        is a function from strings
        to list of pairs
        of things and strings 
        
-}
newtype Parser a 
  = P (String -> [(a,String)])
  
data Item
  = Item {
    itemId :: String,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

