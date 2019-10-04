data User =
  User
    { name :: String
    , gamerId :: Int
    , score :: Int
    }
  deriving (Show)

u1 = User {name = "Sue", gamerId = 1337, score = 9001}

u2 = User "Sue" 1337 9001

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

u3 = User <$> serverUsername <*> serverGamerId <*> serverScore

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Enter a username, gamerId and score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user

u4 = User <$> Nothing <*> serverGamerId <*> serverScore
