-- chapter_exercises.hs --

module ChapterExercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y


flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


-- Recursion --
-- 2 --
rSum :: (Eq a, Num a) => a -> a
rSum n = go n 1
    where go n sum
            | n == 1    = sum
            | otherwise = go (n - 1) (sum + n)


-- 3 --
rMult :: Integral a => a -> a -> a
rMult x y = go x y 0 0
    where go x y count sum
            | count == y = sum
            | otherwise  = go x y (count + 1) (sum + x)


-- McCarthy 91 --
mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91(n + 11))

