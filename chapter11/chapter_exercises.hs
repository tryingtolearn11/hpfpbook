-- chapter_exercises.hs --

module ChapterExercises where

import Data.List
import Data.Char

-- 2 --
data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday


f :: Weekday -> String
f Friday = "Miller Time"



-- Vigenere Ciper --



abc = ['a'..'z']


-- keystream (plaintext) (key) = ...
keystream :: String -> String -> String
keystream xs ys = go xs ys 0 (length ys) " " (length xs)
    where go (x:xs) (ys) count lenKey answerString j
            | x == ' '      = go (xs) (ys) count lenKey (answerString) j
            | count == j    = answerString
            | otherwise     = (ys !! (count `mod` lenKey)) : go (xs) (ys) (count + 1) (lenKey) answerString j
          go [] _ _ _ _ _ = []


position :: Eq a => a -> [a] -> Int
position i xs =
    case i `elemIndex` xs of
      Just n -> n
      Nothing -> 0


-- now apply ord to each char from plaintext and shift them n positions
