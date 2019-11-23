import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef

aLargeList :: [Int]
aLargeList = [1 .. 10000000]

aLargeArray :: UArray Int Int
aLargeArray = array (0, 9999999) []

aLargeListDoubled :: [Int]
aLargeListDoubled = map (* 2) aLargeList

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 9) [(3, True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1, 10) $ zip [1 .. 10] $ cycle [True]

qcArray :: UArray Int Bool
qcArray = array (0, 4) [(1, True), (2, True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) $ zip [0 .. 3] $ cycle [0]

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1, 5), (3, 6)]

addTwoToAllOfThem :: UArray Int Int
addTwoToAllOfThem = accum (+) updatedBiB $ zip [0 .. 3] $ cycle [2]

doubleAllOfThem :: UArray Int Int
doubleAllOfThem = accum (*) updatedBiB $ zip [0 .. 3] $ cycle [2]

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  stArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray stArray i val
  return stArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

listToUArray2 :: [Int] -> UArray Int Int
listToUArray2 vals =
  runSTUArray $ do
    let end = length vals - 1
    stArray <- newArray (0, end) 0
    forM_ [0 .. end] $ \i -> do
      let val = vals !! i
      writeArray stArray i val
    return stArray

swapST :: (Int, Int) -> (Int, Int)
swapST (x, y) =
  runST $ do
    x' <- newSTRef x
    y' <- newSTRef y
    writeSTRef x' y
    writeSTRef y' x
    xfinal <- readSTRef x'
    yfinal <- readSTRef y'
    return (xfinal, yfinal)

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray =
  runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i ->
      forM_ [0 .. (end - i)] $ \j -> do
        val <- readArray stArray j
        nextVal <- readArray stArray (j + 1)
        let outOfOrder = val > nextVal
        when outOfOrder $ do
          writeArray stArray j nextVal
          writeArray stArray (j + 1) val
    return stArray

crossOver :: (UArray Int Int, UArray Int Int) -> Int -> UArray Int Int
crossOver (a1, a2) crossOverPt =
  runSTUArray $ do
    st1 <- thaw a1
    let end = (snd . bounds) a1
    forM_ [crossOverPt .. end] $ \i -> writeArray st1 i $ a2 ! i
    return st1

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros array =
  runSTUArray $ do
    stArray <- thaw array
    let end = (snd . bounds) array
    let count = 0
    forM_ [0 .. end] $ \i -> do
      val <- readArray stArray i
      when (val == 0) $ writeArray stArray i (-1)
    return stArray
