import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

inputs :: Maybe Int String

mainMaybe :: Maybe String
mainMaybe = do
  name <- Map.lookup 1 inputs
  let statement = helloPerson name
  return statement