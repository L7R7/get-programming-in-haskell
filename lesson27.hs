import qualified Data.Map as Map

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just s) = Just (reverse s)
reverseMaybe Nothing = Nothing

-- already defined in GHC.Base
-- instance Functor Maybe where
--   fmap func (Just n) = Just (func n)
--   fmap func Nothing = Nothing
successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

reverseMaybe2 :: Maybe String -> Maybe String
reverseMaybe2 = fmap reverse

reverseMaybe3 :: Maybe String -> Maybe String
reverseMaybe3 m = reverse <$> m

data RobotPart =
  RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm"
    , description = "left arm for face punching!"
    , cost = 10000.00
    , count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.00
    , count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>"
    , partName
    , "</h2>"
    , "<p><h3>desc</h3>"
    , partDesc
    , "</p><p><h3>cost</h3>"
    , partCost
    , "</p><p><h3>count</h3>"
    , partCount
    , "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

-- insertSnippet :: Maybe Html -> IO ()
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allParts2 :: [RobotPart]
allParts2 = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

-- allPartsHtml = fmap renderHtml allParts
-- allPartsHtml = map renderHtml allParts -- for lists, fmap is the same as map
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

data Box a =
  Box a
  deriving (Show)

instance Functor Box where
  fmap f (Box a) = Box (f a)

morePresents :: Box a -> Int -> Box [a]
morePresents box n = take n . repeat <$> box

myBox :: Box Int
myBox = Box 1

unwrap :: Box a -> a
unwrap (Box x) = x

wrapped :: Box (Box Int)
wrapped = fmap Box myBox

unwrapped :: Box Int
unwrapped = fmap unwrap wrapped

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "item not found"
printCost (Just cost) = print cost

main :: IO ()
main = do
  putStrLn "enter a part number"
  partNo <- getLine
  let part = Map.lookup (read partNo) partsDB
  printCost (cost <$> part)
