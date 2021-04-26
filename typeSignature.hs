-- typeSignature.hs

module TypeSignature where

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
    if (x > y) then True else False

functionS :: (x, y) -> y
functionS (x, y) = y
