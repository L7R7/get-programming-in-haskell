type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char
  deriving (Show)

data AuthorName =
  AuthorName FirstName LastName

-- data Book =
--   Book AuthorName String String Int
data Book =
  Book
    { author :: Creator
    , isbn :: String
    , bookTitle :: String
    , bookYear :: Int
    , bookPrice :: Double
    }

-- data AuthorName2 =
--   AuthorName2
--    { firstName :: String
--    , lastName :: String
--    }
data Creator
  = AuthorCreator Author
  | ArtistCreator Artist
  deriving (Show)

data Author =
  Author Name
  deriving (Show)

data Artist
  = Person Name
  | Band String
  deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data VinylRecord =
  VinylRecord
    { artist :: Creator
    , recordTitle :: String
    , recordYear :: String
    , recordPrice :: Double
    }

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

data CollectibleToy =
  CollectibleToy
    { name :: String
    , toyDescription :: String
    , toyPrice :: Double
    }

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price _ = 0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"

data Pamphlet =
  Pamphlet
    { title :: String
    , pamphletDescription :: String
    , contact :: String
    }

data Shape
  = Circle Double
  | Square Double
  | Rectangle Double Double

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square d) = 4 * d
perimeter (Rectangle a b) = 2 * (a + b)

area :: Shape -> Double
area (Circle r) = pi * (r ^ 2)
area (Square d) = d ^ 2
area (Rectangle a b) = a * b
