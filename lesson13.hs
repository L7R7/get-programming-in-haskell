addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
  describe :: a -> String

data Icecream
  = Chocolate
  | Vanilla
  deriving (Show, Eq, Ord)

inc :: Int -> Int
inc x = x + 1

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound -- maxBound and minBound are just values, no functions
    then minBound -- the compiler infers the type automatically from the context (?)
    else succ n
