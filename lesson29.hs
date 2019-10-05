s = (++) <$> Just "Learn" <*> Just "Haskell"

hello :: IO String
hello = pure "hello"

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 2000]

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

boxMultiplier :: [Int]
boxMultiplier = [10, 50]

res :: [Int]
res = pure (*) <*> doorPrize <*> boxMultiplier

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)

data User =
  User
    { name :: String
    , gamerId :: Int
    , score :: Int
    }
  deriving (Show)

testNames :: [String]
testNames =
  [ "John Smith"
  , "Robert'); DROP TABLE Students;--"
  , "Christina NULL"
  , "Randall Munroe"
  ]

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f fa = pure f <*> fa

v1 = allFmap (+ 1) [1, 2, 3]

v2 = allFmap (+ 1) (Just 5)

v3 = allFmap (+ 1) Nothing

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

boughtLastNight :: [Int]
boughtLastNight = [6, 12]

drankLastNight :: Int
drankLastNight = 2 * 2

beersLeft :: [Int]
beersLeft = (-) drankLastNight <$> boughtLastNight

friendsComingTonight :: [Int]
friendsComingTonight = [2, 3]

peopleTonight :: [Int]
peopleTonight = fmap (+ 1) friendsComingTonight

beersEach :: [Int]
beersEach = [3, 4]

beersNeededTonight :: [Int]
beersNeededTonight = pure (*) <*> peopleTonight <*> beersEach

beersToBuy :: [Int]
beersToBuy = pure (-) <*> beersLeft <*> beersNeededTonight
