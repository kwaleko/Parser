module   Parser       where 


import                Control.Applicative ((<|>))
import                Data.Char (isDigit)
import                Data.Either(Either(..)) 
import                Prelude hiding (break,or)
import                Text.Read (readMaybe)


import                Types 


-- parse any character or fail if the end of the given string is reached  
item :: Parser Char
item = Parser (\inp -> case inp of
  []-> Left "item parser : End of string is reached"
  (x:xs) -> Right (x,xs))
 
-- a Parser that succeed if the end of the given string is reached or fail otherwise.
end :: Parser [Char]
end = Parser (\inp -> case inp of
  [] ->  Right ("","")
  otherwise -> Left "end parser : not the end of the string yet"
  )

-- parse given characher
sat :: (Char -> Bool) -> Parser Char
sat f = do
  c <- item
  case  f c of
    False -> fail $ "item parser : could not find character " ++ show c
    True  -> return c 
    
char :: Char -> Parser Char
char x = sat (== x)
     
digit :: Parser Char
digit = sat isDigit

string :: String -> Parser String
string [] = return []
string (x:xs) = char x >> string xs >> return (x:xs)

till :: Char ->  Parser String 
till val = 
   (end >> fail ("till parser : character " ++ show val ++ "is missing at the end")  )
  <|> (char val >> return [])
  <|> ( item >>= \c -> (till val) >>= \cs -> return (c:cs))
   
or = (<|>)

articles :: Parser [Article]
articles =  
     end >> return []
     <|> 
     do
      id      <- till '#'
      title   <- till '#'
      content <- till '#'
      rem     <- articles
      return $ Article (readMaybe id :: Maybe Int) title content : rem
      
article :: String -> Parser Article
article c =  
     do
      end
      fail $ "Could not find the artivle " ++ show c
  `or` 
     do
      id      <- till '#'
      title   <- till '#'
      content <- till '#'
      if id == c 
        then
         return $ Article (readMaybe id :: Maybe Int) title content
        else  article c

       
split :: Char -> Parser [String]
split val =
  (end >> return [])
  <|> (till val >>= \c -> (split val) >>= \cs -> return (c:cs))
  