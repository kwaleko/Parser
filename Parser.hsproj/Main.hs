import Parser
import System.IO

main :: IO ()
main = do
  inpStr <- readFile "/Users/lambda/Documents/createFile.txt"
  let [(result,_)] = parse allDigits inpStr
  putStr result
