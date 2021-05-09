-- chapter_exercises.hs --

module ChapterExercises where

import Data.Char


-- 2 --
onlyUpper :: String -> String
onlyUpper xs = filter (\x -> (isUpper x)) xs

-- 3 --
capitalize :: String -> String
capitalize (x : xs) = (toUpper x) : xs

-- 4 --
capitalize_all :: String -> String
capitalize_all [] = []
capitalize_all (x : xs) = (toUpper x) : capitalize_all xs

-- 5 --
capitalize_head :: [Char] -> Char
capitalize_head xs = toUpper $ (head xs)

-- 6 --
capitalize_head' :: [Char] -> Char
capitalize_head' xs = (toUpper . head) xs

capitalize_head_pointfree :: [Char] -> Char
capitalize_head_pointfree = (toUpper . head)








