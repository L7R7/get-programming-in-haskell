import qualified Data.Map as Map

type UserName = String

type GamerId = Int

type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB =
  Map.fromList
    [ (1, "nYarlathoTep")
    , (2, "KINGinYELLOW")
    , (3, "dagon1997")
    , (4, "rcarter1919")
    , (5, "xCTHULHUx")
    , (6, "yogSOThoth")
    ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB =
  Map.fromList
    [ ("nYarlathoTep", 2000)
    , ("KINGinYELLOW", 15000)
    , ("dagon1997", 300)
    , ("rcarter1919", 12)
    , ("xCTHULHUx", 50000)
    , ("yogSOThoth", 150000)
    ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id

creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = altLookupCredits (lookupUserName id)
creditsFromId id = lookupUserName id >>= lookupCredits

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB =
  Map.fromList
    [(1001, 1), (1002, 2), (1003, 3), (1004, 4), (1005, 5), (1006, 6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

echo :: IO ()
echo = getLine >>= putStrLn

main :: IO ()
main = echo

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

combined :: IO ()
combined = readInt >>= printDouble

echoVerbose :: IO ()
echoVerbose = putStrLn "Give me something to echo!" >> getLine >>= putStrLn

askForName :: IO ()
askForName = putStrLn "What's your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> getLine >>= return . nameStatement >>= putStrLn

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f ma = ma >>= return . f

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf ma = ma >>= (\a -> mf >>= (\f -> return (f a)))

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just a) f = f a
