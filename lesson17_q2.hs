cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (replicate nToAdd) l1
    -- repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

newtype Events =
  Events [String]
  deriving (Show)

instance Semigroup Events where
  (<>) = combineEvents

instance Monoid Events where
  mempty = Events []

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events combined
  where
    combined = cartCombine combiner e1 e2
    combiner x y = mconcat [x, "-", y]

newtype Probs =
  Probs [Double]
  deriving (Show)

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Probs where
  mempty = Probs []

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs combined
  where
    combined = cartCombine (*) p1 p2

data PTable =
  PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sumProbs probs
    normalizedProbs = normalizeProbs probs totalProbs

sumProbs :: Probs -> Double
sumProbs (Probs ps) = sum ps

normalizeProbs :: Probs -> Double -> Probs
normalizeProbs (Probs ps) totalProbs = Probs normalized
  where
    normalized = map (/ totalProbs) ps
    -- normalized = map (\x -> x / totalProbs) ps

instance Show PTable where
  show (PTable (Events e) (Probs p)) = mconcat pairs
    where
      pairs = zipWith showPair e p

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable events probs
    where
      events = mempty :: Events
      probs = mempty :: Probs

coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])
