myDrop 0 xs = xs
myDrop _ [] = []
-- myDrop n xs = myDrop (n-1) (tail xs)
myDrop n (_:xs) = myDrop (n - 1) xs

myLength [] = 0
-- myLength xs = 1 + myLength (tail xs)
myLength (_:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : rest
  where
    rest = myTake (n - 1) xs

finiteCycle (first:rest) = first : rest ++ [first]

myCycle (first:rest) = first : myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

myReverse [] = []
-- myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- fastFib _ n2 0 = n2
-- fastFib n1 n2 counter = fastFib n2 (n1+n2) (counter-1)
fastFib2 _ _ 0 = 0
fastFib2 _ _ 1 = 1
fastFib2 _ _ 2 = 2
fastFib2 x y 3 = x + y
fastFib2 x y c = fastFib2 (x + y) x (c - 1) -- fib n = fastFib2 1 1 n
