-- fibonacci.hs --

module Fibonacci where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


-- Integral Division --


data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go num denom 0 1 
    where go n d count s
            | n < 0 = go (- n) d count (- s)
            | d < 0 = go n (- d) count (- s)
            | n < d = Result (count * s)
            | otherwise = go (n - d) d (count + 1) s
