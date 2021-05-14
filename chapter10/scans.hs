-- scans.hs --

module Scans where

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- 1 --
fibs' = 1 : scanl (+) 1 fibs'
fibsN20' = take 20 $ fibs'


-- 2 --
fibsLTHundred = takeWhile (<100)  fibs'

-- 3 --

factorial 0 = 1
factorial n = n * factorial ( n - 1 )

factorialScan = scanl (*) 1 [1,2..]
factorialScanN x = factorialScan !! (x)
