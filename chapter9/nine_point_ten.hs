-- nine_point_ten.hs --

module NinePointTen where

-- 1 --
multiplsOfThree :: [Integer]
multiplsOfThree = filter (\x -> (rem x 3) == 0) [1..30]


-- 2 --
f :: Int
f = length $ multiplsOfThree

-- 3 --
myFilter :: String -> [String]
myFilter y = filter (\x -> notElem x ["the", "a", "an"]) (words y)














