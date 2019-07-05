gcd1 a b =
  if remainder == 0
    then b
    else gcd1 b remainder
  where
    remainder = a `mod` b

sayAmount n =
  case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

myHead (x:xs) = x
myHead [] = error "No head for empty list"

myTail [] = []
myTail (_:xs) = xs

gcd2 a b =
  case remainder of
    0 -> b
    _ -> gcd2 b remainder
  where
    remainder = a `mod` b

gcd3 a 0 = a
gcd3 a b = gcd3 b (a `mod` b)
-- myTake 0 xs = []
-- myTake n xs = [h, t]
--   where h = head xs
--         t = myTake (n-1) (tail xs)
