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
{-
-- now apply ord to each char from plaintext and shift them n positions
ciper xs keyword = go xs (keystream xs keyword) " " 0 (length xs)
    where go (x : xs) (y : ys) s count j
            | count == j           = s
            | otherwise            = abc !!(((position x abc) + (position y abc)) `mod` 26) : go (xs) (ys) (s) (count + 1) j
          go [] _ _ _ _= []
          go _ [] _ _ _ = []
-}
          

-- As-patterns --
-- TODO: understand as-patterns more
isSubSeqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubSeqOf [] _ = True
isSubSeqOf _ [] = False
isSubSeqOf xs@(x : xz) (y : yz)
  | x == y    = isSubSeqOf xz yz
  | otherwise = isSubSeqOf xs yz



























