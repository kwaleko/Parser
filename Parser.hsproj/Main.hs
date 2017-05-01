import Parser
import File 
import System.IO

 
{- How to use the below function to parse digits in a given file:

  -Create a txt file
  -replace the path in the readFile to be the path for the created file
  -go to cmd
  -run ghci or stack ghci if you are using stack
  -then load the Main file in the REPL ( :load Main.hs )
  -now you can write main in the REPL and it will return the parsed string
-}
main :: IO ()
main = do
  inpStr <- readFile "/Users/lambda/Documents/createFile.txt"
  let [(result,_)] = parse parseDigits inpStr
  putStr result
  



