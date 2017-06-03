import Parser
import File 
import System.IO
import Types

 
{- How to use the below function to parse digits in a given file:

  -Create a txt file
  -replace the path in the readFile to be the path for the created file
  -go to cmd
  -run ghci or stack ghci if you are using stack
  -then load the Main file in the REPL ( :load Main.hs )
  -now you can write main in the REPL and it will return the parsed string
-}
 {- main :: IO String
main = do
  inpStr <- readFile "/Users/lambda/Documents/createFile.txt"
  let [(result,_)] = parse parseDigits inpStr
  return result
  -}
  

  
fn :: String -> [(Char, Char, Char)]                                                                                                                                                                    
fn s = snd $ foldr toTriples ([], []) s where
    toTriples :: Char -> (String, [(Char, Char, Char)]) -> (String, [(Char, Char, Char)])
    toTriples c (cur, tups) | length cur < 2 = (c:cur, tups)
    toTriples c (cur, tups) = ([], (c, cur!!0, cur!!1):tups)