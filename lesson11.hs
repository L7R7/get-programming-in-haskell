x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1, 2, 3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: String -- [Char]
letters = ['a', 'b', 'c']

aPet :: String -- [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")

half :: Int -> Double
-- half n = n/2
half n = fromIntegral n / 2

halve :: Int -> Int
halve n = div n 2

-- halve n = n `div` 2
printDouble :: Int -> String
printDouble n = show (n * 2)

-- printDouble n = show $ n * 2
anotherNumber :: Int
anotherNumber = read "6"

tail2 :: [a] -> [a]
tail2 (x:xs) = xs
tail2 [] = []

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where
    newInit = f init x
