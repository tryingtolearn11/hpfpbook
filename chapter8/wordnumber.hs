-- wordnumber.hs --

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0    = "zero"
  | n == 1    = "one"
  | n == 2    = "two"
  | n == 3    = "three"
  | n == 4    = "four"
  | n == 5    = "five"
  | n == 6    = "six"
  | n == 7    = "seven"
  | n == 8    = "eight"
  | otherwise = "nine"


digits :: Int -> [Int]
digits n = go n (n, 0) [] 0 
    where go n a k count
            | fst a == 0 = take count $ (snd a:k)
            | otherwise  = go n (fst a `divMod` 10) ((snd a):k) (count + 1)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" $ ((map digitToWord . digits ) n)



