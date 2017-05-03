module Parser (parse,parseDigits) where 

import Control.Applicative 
import Data.Char
--import Types

{- a parser for things
is a function from strings
to list of pairs
of things and strings -}
newtype Parser a 
  = P (String -> [(a,String)])

-- Type of tags available in the text file.  
data TextType 
  = Title
  | Body
  deriving Show
  
data Post 
  = Post String String
  deriving Show
  
--function that remove the constructer and return the parser result.
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


-- parser of character, it is the basic block in which all other parser that that consume one character will be constructed. 
item :: Parser Char
item = P (\inp -> case inp of
  [] -> []
  (x:xs) -> [(x,xs)])
 

-- a Parser that succeed if the end of the given string is reached or fail otherwise.
end :: Parser [Char]
end = P (\inp -> case inp of
  [] -> [("","")]
  otherwise -> []
  )
  

-- fmap applies a funtion to the result value of the parser if the parser succeed, and propagates the failure otherwise.
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\inp -> case parse p inp of
    [] -> []
    [(v,out)] -> [(f v,out)])

{- pure transorm a value that always succeed with this value as its result, without consuming any of the input string 

  <*> applies a parser that returns a function to a parser that returns an argument to give a parser that 
  returns the result of applying the function to the argument, and only succed if all the compponents succeed 
-} 
    
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


-- a parse that take a predicate and parse the first character if the predicate is evluated to true and fail otherwise.
sat :: (Char -> Bool) -> Parser Char
sat f = item >>= \x -> case f x of
  False -> fail "error"
  True -> return x 
 
 
instance Alternative Parser where
  --empty :: Parser a
  empty = P (\inp -> [])
  
  -- <|> Parse a -> Parser a -> Parser a
  p1 <|> p2 = P (\inp -> case parse p1 inp of 
    [] ->  parse p2 inp
    [(v,out)] -> [(v,out)])
 
  
-- a parser that parser only digits.
digit :: Parser Char
digit = sat isDigit


-- if the character is digit the function will return True, False otherwise.
isNotDigit :: Char -> Bool
isNotDigit x = not $ isDigit x

-- a parser that parser a specific char or fail otherwise.
char :: Char -> Parser Char
char x = sat (== x)

-- string is a parser that parse a specific string
string :: String -> Parser String
string [] = return []
string (x:xs) = char x >> string xs >> return (x:xs)


{- parseDigits is a parser of string that parse all digit in a given string using the below method:
-try a parser  (end >>= \_ -> return "") that check if the list of character has been consumed, if this parser
 fail then the next parser will be executed, if succeeded then it will return a parser of empty string with the
  remaining string is empty as well.

-the second parser is ((sat isNotDigit) >>= \_ -> ) that try to parse a character and succeed as long 
as the character is not digit, or fail otherwise. when succeeded the parsed character will ignore by monadic bind 
then the function allDigit will be called again, when it fails then it will try the next parser.

-the thrid parser ((many digit) >>= \c ->   >>= \cs -> return (c++cs)) will try to parser list of 
digits, and this function will keep calling itself recusively till the end of the string then return the parsed 
characters and return it using the succeed parser. 
-}
parseDigits :: Parser [Char]
parseDigits = end  
  <|> ((sat isNotDigit) >> parseDigits) 
  <|> ((many digit) >>= \c -> parseDigits >>= \cs -> return (c++cs))



{- Parser of string that parse a string till it attend a given character then stop.
for example : parser (ParseTill ';') "abc;def" will return [("abc","def")].
-}
parseTill :: Char ->  Parser String 
parseTill char = end
  <|> (sat (== char) >> return [])
  <|> ( item >>= \c -> (parseTill char) >>= \cs -> return (c:cs))
  
{- Parser of string that split the string based on semi-colon.
ex :
"title;post;title2;post" -> ["title","post","title2","post"]
-}
parsePosts :: Parser [String]
parsePosts =
  (end >> return [])
  <|> (parseTill ';' >>= \c -> parsePosts >>= \cs -> return (c:cs))
  
-- Convert a list of string to list of Post.
beautify :: [String] -> [Post]
beautify str = case str of 
  [] -> []
  (title:body:xs) -> (Post title body) : beautify xs
  












  
