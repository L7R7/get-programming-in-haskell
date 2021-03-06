data FourLetterAlphabet
  = L1
  | L2
  | L3
  | L4
  deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a -- p.158: because of type classes, it also works for instance for Bools out of the box
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

largestCharNumber :: Int -- not a function, just a value!
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar = rotN sizeOfAlphabet
  where
    sizeOfAlphabet = 1 + largestCharNumber

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map rot4l
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4l = rotN alphaSize

data ThreeLetterAlphabet
  = Alpha
  | Beta
  | Kappa
  deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder = map rot3l
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3l = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset =
      if even n
        then fromEnum c + halfN
        else 1 + fromEnum c + halfN
    rotation = offset `mod` n

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder = map rot3ldecoder
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3ldecoder = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder = map rotChar
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotCharDecoder = rotNdecoder alphaSize

threeLetterEncoder2 :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder2 = map rot3l
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3l = rotN alphaSize

threeLetterDecoder2 :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder2 = map rot3ldecoder
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3ldecoder = rotNdecoder alphaSize

-- TODO: check xor in Data.Bool
xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not v1 && v2)

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair $ zip list1 list2

type Bits = [Bool]

-- my (partially spoiled) version:
-- intToBits' :: Int -> Bits
-- intToBits' 0 = [False] -- i didn't come up with this trick
-- intToBits' 1 = [True]
-- intToBits' n = if n `mod` 2 == 0
--                   then False : (intToBits' (n `div` 2))
--                   else True : (intToBits' (n `div` 2))
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - length reversedBits
    leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits $ fromEnum char

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
  where
    size = length bits
    indices = [size - 1,size - 2 .. 0]
    trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =
  map (\pair -> fst pair `xor` snd pair) (zip padBits plaintextBits)
  where
    padBits = map charToBits pad
    plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where
    bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot =
  Rot

instance Cipher Rot where
  encode Rot = rotEncoder
  decode Rot = rotDecoder

newtype OneTimePad =
  OTP String

instance Cipher OneTimePad where
  encode (OTP pad) = applyOTP pad
  decode (OTP pad) = applyOTP pad

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber
