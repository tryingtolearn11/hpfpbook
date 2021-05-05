-- enumfromto.hs --

module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True  = [False, True]
eftBool True False  = []
eftBool True True   = [True]




eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : (eftOrd (succ x) y)


eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
    where go x y k
            | x > y     = []
            | x == y    = reverse (x:k)
            | otherwise = go (x + 1) y (x : k)

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
    where go x y k
            | x > y     = []
            | y == x    = y : k
            | otherwise = go (x) (pred y) (y:k)
