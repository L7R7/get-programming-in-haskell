module Primes
  ( primes
  , isPrime
  , primeFactors
  ) where

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where
    noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n
  | n < 2 = Nothing
  | n > maximum primes = Nothing
  | otherwise = Just (n `elem` primes)

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n > maximum primes = Nothing
  | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where
    primesLessThanN = filter (<= n) primes

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) =
  if n `mod` next == 0
    then next : unsafePrimeFactors (n `div` next) (next : primes)
    else unsafePrimeFactors n primes
