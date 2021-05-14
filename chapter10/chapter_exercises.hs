-- chapter_exercises.hs --

module ChapterExercises where

-- 1 --
-- function broken idk
-- TODO : come back to solve this lol
f [] _ = []
f (_:[]) _ = []
f (x : xy) z = (x, z, x) : (x, z, (head xy)) : (head xy, z, x) : (head xy, z, last xy) :(x, z, last xy) : f (xy) z

g = map . f

-- 2 --
xs :: String
xs = "I love cats"


seekrItFunc :: String -> Int
seekrItFunc x =
    div (sum (map length (words x))) (length (words x))


-- 3 --
seekrItFunc' :: Fractional a => String -> a
seekrItFunc' x = (fromIntegral(sum (map length (words x)))) / (fromIntegral (length (words x)))

-- Rewriting --
-- 1 --
myOr :: [Bool] -> Bool
myOr = foldr (||) False


-- 2 --
myAny :: (a -> Bool) -> [a] -> Bool 
myAny f = foldr ((||) . f) False

-- 3 --
myElem :: Eq a => a -> [a] -> Bool
myElem x ys = foldr ((||) . (== x)) False ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = any (==x) ys

