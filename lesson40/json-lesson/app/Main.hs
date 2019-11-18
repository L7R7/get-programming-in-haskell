module Main where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy       as B
import           Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  as T
import           GHC.Generics

data Book =
  Book
    { title  :: T.Text
    , author :: T.Text
    , year   :: Int
    }
  deriving (Generic, Show)

instance FromJSON Book

instance ToJSON Book

myBook :: Book
myBook = Book {author = "Will Kurt", title = "Learn Haskell", year = 2017}

myBookJson :: BC.ByteString
myBookJson = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\", \"title\": \"A Short History of Decay\", \"year\": 1949}"

wrongJSON :: BC.ByteString
wrongJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year=1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

bookFromWrongJSON :: Either String Book
bookFromWrongJSON = eitherDecode wrongJSON

data Name =
  Name
    { firstName :: T.Text
    , lastName  :: T.Text
    }
  deriving (Show)

instance FromJSON Name where
  parseJSON (Object v) = Name <$> v .: "firstName" <*> v .: "lastName"

instance ToJSON Name where
  toJSON (Name firstName lastName) = object ["firstName" .= firstName, "lastName" .= lastName]

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage =
  ErrorMessage
    { message   :: T.Text
    , errorCode :: Int
    }
  deriving (Show)

instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) = object ["message" .= message, "error" .= errorCode]

exampleMessage :: Maybe T.Text
exampleMessage = Just "Opps"

exampleError :: Maybe Int
exampleError = Just 123

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0

data NOAAResult =
  NOAAResult
    { uid          :: T.Text
    , mindate      :: T.Text
    , maxdate      :: T.Text
    , name         :: T.Text
    , datacoverage :: Float
    , resultId     :: T.Text
    }
  deriving (Show)

instance FromJSON NOAAResult where
  parseJSON (Object v) = NOAAResult <$> v .: "uid" <*> v .: "mindate" <*> v .: "maxdate" <*> v .: "name" <*> v .: "datacoverage" <*> v .: "id"

instance ToJSON NOAAResult where
  toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
    object ["uid" .= uid, "mindate" .= mindate, "maxdate" .= maxdate, "name" .= name, "datacoverage" .= datacoverage, "id" .= resultId]

data Resultset =
  Resultset
    { offset :: Int
    , count  :: Int
    , limit  :: Int
    }
  deriving (Show, Generic)

instance FromJSON Resultset

instance ToJSON Resultset

newtype Metadata =
  Metadata
    { resultset :: Resultset
    }
  deriving (Show, Generic)

instance FromJSON Metadata

instance ToJSON Metadata

data NOAAResponse =
  NOAAResponse
    { metadata :: Metadata
    , results  :: [NOAAResult]
    }
  deriving (Show, Generic)

instance FromJSON NOAAResponse

instance ToJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing        = print "error loading data"
printResults (Just results) = forM_ results (print . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults

data IntList
  = Cons Int IntList
  | EmptyList
  deriving (Show, Generic)

instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList
