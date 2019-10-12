import Control.Monad
import Data.Char

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2 ^ value)

powersOfTwo2 :: Int -> [Int]
powersOfTwo2 n = [value ^ 2 | value <- [1 .. n]]

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2 ^ x) [1 .. n]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

powersOfTwoAndThree2 :: Int -> [(Int, Int)]
powersOfTwoAndThree2 n =
  [ (powersOfTwo, powersOfThree)
  | value <- [1 .. n]
  , let powersOfTwo = 2 ^ value
  , let powersOfThree = 3 ^ value
  ]

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2,4 .. n]
  oddValue <- [1,3 .. n]
  return (evenValue, oddValue)

allEvenOdds2 :: Int -> [(Int, Int)]
allEvenOdds2 n =
  [(evenValue, oddValue) | evenValue <- [2,4 .. n], oddValue <- [1,3 .. n]]

numberAndSquare :: [(Int, Int)]
numberAndSquare = do
  number <- [1 .. 10]
  let square = number ^ 2
  return (number, square)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value

evensGuard2 :: Int -> [Int]
evensGuard2 n = [value | value <- [1 .. n], even value]

guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter f as = do
  value <- as
  guard (f value)
  return value

evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n ^ 2
  guard (even nSquared)
  return nSquared

input :: [String]
input = ["brown", "pink", "blue", "orange"]

answer :: [String]
-- answer = ["Mr. " ++ upper | i <- input, let upper = toUpper (head i) : tail i]
answer =
  ["Mr. " ++ upper | i <- input, let upper = (\(x:xs) -> toUpper x : xs) i]

daysInMonth :: [Int]
daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates ends = [date | end <- ends, date <- [1 .. end]]

dates2 :: [Int] -> [Int]
dates2 ends = do
  end <- ends
  [1 .. end]

dates3 :: [Int] -> [Int]
dates3 ends = ends >>= (\end -> [1 .. end])
