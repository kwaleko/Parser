import Parser
import File 
import System.IO

-- return the parsed digits in the given file  
main1 :: IO ()
main1 = do
  inpStr <- readFile "/Users/lambda/Documents/createFile.txt"
  let [(result,_)] = parse parseDigits inpStr
  putStr result
  



