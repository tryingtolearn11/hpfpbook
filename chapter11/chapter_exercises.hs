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
keystream xs ys = go xs ys 0 (length ys) " " (length xs)
    where go (x:xs) (ys) c k s e
            | x == ' '  = go (x:xs) (ys) c k s e
            | c == e    = s
            | otherwise = (ys !! (c `mod` k)) : go (xs) (ys) (c + 1) (k) s e
          go [] _ _ _ _ _ = []
