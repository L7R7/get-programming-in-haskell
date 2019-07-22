data SixSidedDie
  = S1
  | S2
  | S3
  | S4
  | S5
  | S6

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

data TwoSidedDie
  = One
  | Two
  deriving (Show)

show :: TwoSidedDie -> String
show One = "one"
show Two = "two"

instance Eq SixSidedDie where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _ = False

instance Ord SixSidedDie where
  compare S6 S6 = EQ
  compare S6 _ = GT
  compare _ S6 = LT
  compare S5 S5 = EQ
  compare S5 _ = GT
  compare _ S5 = LT
  compare S4 S4 = EQ
  compare S4 _ = GT
  compare _ S4 = LT
  compare S3 S3 = EQ
  compare S3 _ = GT
  compare _ S3 = LT
  compare S2 S2 = EQ
  compare S2 _ = GT
  compare _ S2 = LT
  compare S1 S1 = EQ

data SixSidedDie2
  = D1
  | D2
  | D3
  | D4
  | D5
  | D6
  deriving (Show, Eq, Ord)

instance Enum SixSidedDie where
  toEnum 0 = S1
  toEnum 1 = S2
  toEnum 2 = S3
  toEnum 3 = S4
  toEnum 4 = S5
  toEnum 5 = S6
  toEnum _ = error "No such value"
  fromEnum S1 = 0
  fromEnum S2 = 1
  fromEnum S3 = 2
  fromEnum S4 = 3
  fromEnum S5 = 4
  fromEnum S6 = 5

--data Name = Name (String, String) deriving (Show, Eq)
newtype Name =
  Name (String, String)
  deriving (Show, Eq)

names :: [Name]
names =
  [ Name ("Emil", "Cioran")
  , Name ("Eugene", "Thacker")
  , Name ("Friedrich", "Nietzsche")
  ]

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

data FiveSidedDie
  = F1
  | F2
  | F3
  | F4
  | F5
  deriving (Show, Eq, Ord, Enum)

class (Eq a, Ord a, Enum a) =>
      Die a
  where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum $ mod n 5

instance Die SixSidedDie where
  roll n = toEnum (mod n 6)
