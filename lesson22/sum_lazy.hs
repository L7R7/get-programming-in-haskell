import Data.List.Split

oldMain :: IO ()
oldMain = do
  userInput <- getContents
  mapM_ print userInput

reverser :: IO ()
reverser = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed

myLines = splitOn "\n"

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  -- let transformed = map id numbers
  let transformed = map squared numbers
  print (sum transformed)

squared :: Num a => a -> a
squared a = a ^ 2

calc :: [String] -> Int
calc (val1:"+":val2:rest) = read val1 + read val2
calc (val1:"*":val2:rest) = read val1 * read val2

main1 :: IO ()
main1 = do
  userInput <- getContents
  let values = lines userInput
  print (calc values)

quotes :: [String]
quotes = ["lorem", "ipsum", "dolor", "sit", "amet"]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":_) = []
lookupQuote (x:xs) = quote : lookupQuote xs
  where
    quote = quotes !! index
    index = (read x -1) `mod` length quotes

main2 :: IO ()
main2 = do
  userInput <- getContents
  mapM_ putStrLn (lookupQuote (lines userInput))
