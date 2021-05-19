-- chapter_exercises.hs --

module ChapterExercises where

-- 2 --
data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday


f :: Weekday -> String
f Friday = "Miller Time"



-- Vigenere Ciper --
-- keystream (plaintext) (key) = ...
keystream xs ys = go xs ys 0 (length ys) " " (length xs)
    where go (x:xs) (ys) count lenKey answerString j
            | x == ' '      = go (xs) (ys) count lenKey (answerString) j
            | count == j    = answerString
            | otherwise     = (ys !! (count `mod` lenKey)) : go (xs) (ys) (count + 1) (lenKey) answerString j
          go [] _ _ _ _ _ = []
