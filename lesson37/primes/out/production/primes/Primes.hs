module Primes where

primes :: [Int]
primes = [1 ..]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where
    noFactors = filter ((/= 0) . (`mod` nextPrime)) rest
