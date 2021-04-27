{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

f1 = (* 9) 6

f2 = head[(0, "doge"), (1, "kitten")]

f3 = head [(0 :: Integer, "doge"), (1, "kitten")]

f4 = if False then True else False

f5 = length [1, 2, 3, 4 ,5]

f6 = (length [1, 2, 3, 4]) > (length "TACOCAT")




