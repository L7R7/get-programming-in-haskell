import qualified Data.Map as Map

data RobotPart =
  RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    }
  deriving (Show)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3, 4, 5]
    vals = [leftArm, rightArm, leftLeg, rightLeg, robotShoulder]
    keyVals = zip keys vals

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

leftLeg :: RobotPart
leftLeg =
  RobotPart
    { name = "left leg"
    , description = "left leg for walking!"
    , cost = 9000.00
    , count = 3
    }

rightLeg :: RobotPart
rightLeg =
  RobotPart
    { name = "right leg"
    , description = "right leg for kicking"
    , cost = 425.00
    , count = 5
    }

robotShoulder :: RobotPart
robotShoulder =
  RobotPart
    { name = "robot shoulder"
    , description = "broken shoulder"
    , cost = 92.25
    , count = 2
    }

lowerCost :: RobotPart -> RobotPart -> RobotPart
lowerCost p1 p2 =
  if c1 < c2
    then p1
    else p2
  where
    c1 = cost p1
    c2 = cost p2

readInt :: IO Int
readInt = read <$> getLine

showResult :: Maybe RobotPart -> String
showResult (Just r) = "The part with the lower cost is: " ++ show r
showResult Nothing =
  "There was no matching part in the DB for at least one part ID"

main :: IO ()
main = do
  putStrLn "Please enter two part IDs"
  putStrLn "Part ID 1:"
  pId1 <- readInt
  let part1 = Map.lookup pId1 partsDB
  putStrLn "Part ID 2:"
  pId2 <- readInt
  let part2 = Map.lookup pId2 partsDB
  let lower = lowerCost <$> part1 <*> part2
  putStrLn (showResult lower)
