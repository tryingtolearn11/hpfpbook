-- chapter_exercises.hs --

module ChapterExercises where

-- 1 --
tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = snd y
    where z  = x `div` 10
          y  = z `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = snd j
    where k = x `div` 100
          j = k `divMod` 10

-- or
hunsD' :: Integral a => a -> a
hunsD' = \x -> snd $ ((x `div` 100) `divMod` 10)


-- 2 --
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y


foldBool :: a -> a -> Bool -> a
foldBool x y z
  | z         = y
  | otherwise = x


foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z =
    case z of
      True -> y
      False -> x

-- 3 --
g :: (a -> b) -> (a, c) -> (b, c)
g h (a, c) = (h a, c)

-- or
g' :: (a -> b) -> (a, c) -> (b, c)
g' f x = (f . fst $ x, snd $ x)
