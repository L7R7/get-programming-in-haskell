sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

body sumSquare squareSum =
  if sumSquare > squareSum
    then sumSquare
    else squareSum

sumSquareOrSquareSu2 x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

-- bod2 a b =
--   (\sumSquare squareSum ->
--      if sumSquare > squareSum
--        then sumSquare
--        else squareSum)
--     a
--     b
bod2 sumSquare squareSum =
  if sumSquare > squareSum
    then sumSquare
    else squareSum

sumSquareOrSquareSu3 x y = bod2 (x ^ 2 + y ^ 2) ((x + y) ^ 2)

sumSquareOrSquareSu4 x y =
  (\sumSquare squareSum ->
     if sumSquare > squareSum
       then sumSquare
       else squareSum)
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)

doubleDouble x = dubs * 2
  where
    dubs = x * 2

-- doubleDoubl2 x = (\dubs -> dubs * 2) x * 2
doubleDoubl2 x = (* 2) x * 2

sumSquareOrSquareSu5 x y =
  let sumSquare = (x ^ 2 + y ^ 2)
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum
        then sumSquare
        else squareSum

overwrite x =
  let x = 2
   in let x = 3
       in let x = 4
           in x

overwrit2 x = (\x -> (\x -> (\x -> x) 4) 3) 2

x = 4

add1 y = y + x

add2 y = (\x -> y + x) 3

add3 y = (\y -> (\x -> y + x) 1) 2

-- simple = (\x -> x)
simple x = x

makeChange owed given =
  if given - owed > 0
    then given - owed
    else 0

-- inc = (\x -> x + 1)
inc x = x + 1

-- double = (\x -> x * 2)
double x = x * 2

-- square = (\x -> x * x)
square x = x * x

counter x =
  let x = x + 1
   in let x = x + 1
       in x

-- counte2 x = (\x -> x + 1) ((\x -> x + 1) x)
counte2 x = (+ 1) ((+ 1) x)

-- counte3 x = (\x -> x + 1) ((\x -> x + 1) ((\x -> x) x))
counte3 x = (+ 1) ((+ 1) ((\x -> x) x))
