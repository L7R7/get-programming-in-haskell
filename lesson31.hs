import qualified Data.Map as Map

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM maa = maa >>= pure . uncurry max

maxPairM2 :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM2 maa = do
  (a1, a2) <- maa
  let m = max a1 a2
  pure m

echo :: IO ()
echo = getLine >>= putStrLn

echo2 :: IO ()
echo2 = do
  line <- getLine
  putStrLn line

data Grade
  = F
  | D
  | C
  | B
  | A
  deriving (Eq, Show, Ord, Enum, Read)

data Degree
  = HS
  | BA
  | MS
  | PhD
  deriving (Eq, Show, Ord, Enum, Read)

data Candidate =
  Candidate
    { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree
    }
  deriving (Show)

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

c :: Candidate
c = Candidate 1 B C PhD

readInt :: IO Int
readInt = getLine >>= return . read

readGrade :: IO Grade
readGrade = getLine >>= return . read

readGrade2 :: IO Grade
readGrade2 = do
  line <- getLine
  let grade = read line
  return grade

readDegree :: IO Degree
readDegree = getLine >>= return . read

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return
    (Candidate
       { candidateId = cId
       , codeReview = codeGrade
       , cultureFit = cultureGrade
       , education = degree
       })

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

candidate1 :: Candidate
candidate1 =
  Candidate {candidateId = 1, codeReview = A, cultureFit = A, education = BA}

candidate2 :: Candidate
candidate2 =
  Candidate {candidateId = 2, codeReview = C, cultureFit = A, education = PhD}

candidate3 :: Candidate
candidate3 =
  Candidate {candidateId = 3, codeReview = A, cultureFit = B, education = MS}

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

failPassOrElse :: Maybe String -> String
failPassOrElse (Just s) = s
failPassOrElse Nothing = "error: id not found"

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

assessCandidates :: [Candidate] -> [String]
assessCandidates candidates =
  map
    (\x ->
       if x
         then "passed"
         else "failed")
    passed
  where
    passed = map viable candidates

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

candidateIO = assessCandidate readCandidate

candidateMaybe = assessCandidate (Map.lookup 1 candidateDB)

candidateMaybe2 = assessCandidate (Map.lookup 2 candidateDB)

candidateMaybe3 = assessCandidate (Map.lookup 3 candidateDB)

candidateMaybe4 = assessCandidate (Map.lookup 4 candidateDB)

candidateList = assessCandidate candidates

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerInch p1
    costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The " ++
  show size ++
  " pizza " ++ "is cheaper at " ++ show costSqInch ++ " per square inch"
  where
    costSqInch = costPerInch (size, cost)

mainPizza :: IO ()
mainPizza =
  putStrLn "What is the size of pizza 1" >> getLine >>=
  (\size1 ->
     putStrLn "What is the cost of pizza 1" >> getLine >>=
     (\cost1 ->
        putStrLn "What is the size of pizza 2" >> getLine >>=
        (\size2 ->
           putStrLn "What is the cost of pizza 2" >> getLine >>=
           (\cost2 ->
              (\pizza1 ->
                 (\pizza2 ->
                    (putStrLn . describePizza) (comparePizzas pizza1 pizza2))
                   (read size2, read cost2))
                (read size1, read cost1)))))

mainList :: [Pizza] -> [String]
mainList pizza = do
  size1 <- [10, 12, 17]
  cost1 <- [12.0, 15.0, 20.0]
  size2 <- [10, 11, 18]
  cost2 <- [13.0, 14.0, 21.0]
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

mainMonad :: Monad m => m Pizza -> m Pizza -> m String
mainMonad m1 m2 = do
  pizza1 <- m1
  pizza2 <- m2
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

mainMonad2 ::
     Monad m => m Double -> m Double -> m Double -> m Double -> m String
mainMonad2 s1 c1 s2 c2 = do
  size1 <- s1
  cost1 <- c1
  size2 <- s2
  cost2 <- c2
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
