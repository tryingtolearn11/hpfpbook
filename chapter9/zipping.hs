-- zipping.hs --

module Zipping where

zip1 :: [a] -> [b] -> [(a, b)]
zip1 _ [] = []
zip1 [] _ = []
zip1 (x : xs) (y : ys) = (x, y) : zip xs ys


zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f _ [] = []
zipWith1 f [] _ = []
zipWith1 f (x : xs) (y : ys) = (f x y) : zipWith1 f xs ys
