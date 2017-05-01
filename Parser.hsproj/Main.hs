import Parser
import File 
import System.IO

main :: IO ()
main = do
  inpStr <- readFile "/Users/lambda/Documents/createFile.txt"
  let [(result,_)] = parse allDigits inpStr
  putStr result
  
main1 :: IO ()
main1 = do
  inpStr <- readFile "/Users/lambda/Documents/createFile.txt"
  let [(result,_)] = parse parseDigits inpStr
  putStr result
  


displaySubDirectory :: IO ()
displaySubDirectory = do
  dirs <- countEntries "/Users/lambda/Music/untitled folder 2"
  print dirs


