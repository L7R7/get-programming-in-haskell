import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment
import System.Random

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 256

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

randomChar :: IO Char
randomChar = do
  randomInt <- randomRIO (0, 255)
  return (toEnum randomInt)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size = modifySection start size (BC.reverse . BC.sort)

-- sortSection start size bytes = modifySection start size (BC.reverse . BC.sort) bytes
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection = randomModifySection sortSection

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size = modifySection start size BC.reverse

-- reverseSection start size bytes = modifySection start size BC.reverse bytes
randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection = randomModifySection reverseSection

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte
  , randomSortSection
  , randomSortSection
  , randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomReverseSection
  ]

modifySection ::
     Int
  -> Int
  -> (BC.ByteString -> BC.ByteString)
  -> BC.ByteString
  -> BC.ByteString
modifySection start size f bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = f target

randomModifySection ::
     (Int -> Int -> BC.ByteString -> BC.ByteString)
  -> BC.ByteString
  -> IO BC.ByteString
randomModifySection f bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (f start sectionSize bytes)
