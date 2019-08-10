import Data.List
import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
-- myAll testFunc = (foldr (&&) True) . (map testFunc)
myAll testFunc = foldr ((&&) . testFunc) True

myAny :: (a -> Bool) -> [a] -> Bool
-- myAny testFunc = (foldr (||) False) . (map testFunc)
myAny testFunc = foldr ((||) . testFunc) False

instance Semigroup Integer where
  (<>) = (+)

--  (<>) x y = x + y
data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | White
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a White = a
  (<>) White a = a
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

instance Monoid Color where
  mempty = White

howMuch :: Int -> String
howMuch n
  | n > 10 = "a whole bunch"
  | n > 0 = "not much"
  | otherwise = "we're in debt!"

type Events = [String]

type Probs = [Double]

data PTable =
  PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (replicate nToAdd) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e1
  where
    combiner x y = mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []

--  mappend = (<>) -- actually not needed, because Monoid DOES need a Semigroup instance, so the minimal complete definition is `mempty` only
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]
