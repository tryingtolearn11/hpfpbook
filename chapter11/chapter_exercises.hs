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


-- f :: Weekday -> String
-- f Friday = "Miller Time"

-- 4 --
-- the function g delivers the final element from a list
g :: [a] -> a
g xs = xs !! (length xs - 1)

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
{-

   As-patterns are a nifty way to be able to pattern match
   on part of something and still refer to the entire original value.

-}

-- Practice --
-- Here we pattern matched on a tuple so we could get at the first value
-- for printing, but used the @ symbol to introduce a binding named 't' in
-- order to refer to the whole tuple rather than just a part.

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t

-- Without As-Pattern
f' :: Show a => (a, b) -> IO (a, b)
f' (a, b) = do
    print a
    return (a, b)


-- We use As-Patterns w/ pattern matching here on arbitrary data constructors
-- (lists in this case)
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs


-- Without As-Pattern
doubleUp' :: [a] -> [a]
doubleUp' [] = []
doubleUp' (x: zy) = x : (x : zy)













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




