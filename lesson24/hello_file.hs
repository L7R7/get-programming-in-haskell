import System.IO

main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <-
    if hIsEOF helloFile
      then return "empty"
      else hGetLine helloFile
  putStrLn firstLine
  secondLine <-
    if hIsEOF helloFile
      then return "empty"
      else hGetLine helloFile
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"
