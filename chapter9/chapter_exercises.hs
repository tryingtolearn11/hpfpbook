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




-- Writing standard func --

-- 1 --

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs)
  | x == True = True
  | otherwise = myOr xs

-- 2 --
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x : xs)
  | f x == True = True
  | otherwise   = myAny f xs

-- 3 --
myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y : ys)
  | x == y    = True
  | otherwise = myElem x ys

myElem' :: (a -> Bool) -> [a] -> Bool
myElem' h xs = any h xs

-- 4 --
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = (myReverse xs) ++ [x]

-- 5 --
squish ::  [[a]] -> [a]
squish xs = concat xs

-- 6 --
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = (f x) ++ squishMap f xs

-- 7 --
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8 --
myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy _ [] = error "empty"
myMaximumBy f (x : xs) = go f xs x
    where go _ [] z = z
          go f (y : ys) z = 
              case f y z of 
                GT -> go f ys y
                LT -> go f ys z
                EQ -> go f ys y






-- 9 --
myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy _ [] = error "empty"
myMinimumBy f (x : xs) = go f xs x
    where go _ [] z = z
          go f (y : ys) z =
              case f y z of
                LT -> go f ys y
                GT -> go f ys z
                EQ -> go f ys y


































