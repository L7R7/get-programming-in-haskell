import qualified Data.Map as Map

data Box a =
  Box a
  deriving (Show)

data Triple a =
  Triple a a a
  deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

data List a
  = Empty
  | Cons a (List a)
  deriving (Show)

listEx :: List Int
listEx = Cons 1 (Cons 2 (Cons 3 Empty))

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons a rest) = Cons (f a) (listMap f rest)

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq, Ord, Enum, Bounded)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen] -- enumerate :: Organ

organCounts :: [Int]
organCounts = map countOrgan allOrgans
  where
    countOrgan organ = (length . filter (== organ)) values

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)
