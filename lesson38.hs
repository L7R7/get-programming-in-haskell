import Data.Char

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = []
myTakePM n (x:xs) = x : myTakePM (n - 1) xs

myHead :: [a] -> a
myHead [] = error "emtpy list"
myHead (x:_) = x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) =
  (:) <$> maybeHead xs <*> myTakeSafer (n - 1) (Just (tail xs))

primes :: [Int]
primes = [2, 3, 5, 7]

maxN :: Int
maxN = 10

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

data PrimeError
  = TooLarge
  | InvalidValue

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

instance Show PrimeError where
  show TooLarge = "Value exceeds max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1, 2, 3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: String
charExample = "cat"

charExampleEmpty :: String
charExampleEmpty = ""

example1 :: Either String Int
example1 = (+ 1) <$> eitherHead intExample

example2 :: Either String Int
example2 = (+) <$> eitherHead intExample <*> eitherHead (tail intExample)

main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  print (displayResult result)

allDigits :: String -> Bool
allDigits s = all (== True) (map isDigit s)

addStrInts :: String -> String -> Either String Int
addStrInts s1 s2
  | allDigits s1 && allDigits s2 = Left "both strings are not a number"
  | allDigits s2 = Left "second string is not a number"
  | allDigits s1 = Left "first string is not a number"
  | otherwise = Right ((+) (read s1) (read s2))

succSafe :: (Enum a, Bounded a, Eq a) => a -> Maybe a
succSafe a
  | a == maxBound = Nothing
  | otherwise = Just (succ a)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

safeLast :: [a] -> Either a String
safeLast [] = Right "empty list"
safeLast xs = safeLast' 10000 xs

safeLast' :: Int -> [a] -> Either a String
safeLast' 0 _ = Right "List exceeds safe bound"
safeLast' _ [x] = Left x
safeLast' n (_:xs) = safeLast' (n - 1) xs
