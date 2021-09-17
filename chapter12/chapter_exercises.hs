-- chapter_exercises.hs --


module ChapterExercises where

import Data.Char(toLower)

-- String Processing --

-- 1 --
-- helper function: notThe
notThe :: String -> Maybe String
notThe xs 
  | xs == "the" = Nothing
  | otherwise   = Just xs

replaceThe [] = []
replaceThe xs = unwords $ map f (words xs)
    where f s = case notThe s of
                          Just s -> s
                          Nothing -> "a"



-- 2 --
-- If we found "the", check the element ahead for initial vowel
-- If no "the" found, move on and recurse

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = g (words xs)
    where g [] = 0
          g ("the" : y : ys)   
            | (head y) `elem` "aeiou" = g (ys) + 1
            | otherwise               = g (y:ys)
          g (y : ys) = g (ys) 


-- 3 --
countVowels :: String -> Integer
countVowels [] = 0
countVowels (x : xs)
  | (toLower x) `elem` "aeiou" = countVowels (xs) + 1
  | otherwise        = countVowels (xs)



-- Validate the word --

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"


countConsonants :: String -> Integer
countConsonants [] = 0
countConsonants (x : xs)
  | (toLower x) `notElem` vowels && (x /= ' ') = countConsonants (xs) + 1
  | otherwise                                   = countConsonants (xs)



mkWord :: String -> Maybe Word'
mkWord xs = if (countVowels xs > countConsonants xs) then Nothing else Just (Word' xs)



-- Natural --
data Nat = Zero | Succ Nat deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a






