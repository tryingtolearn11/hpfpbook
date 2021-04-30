-- grabBags.hs

module GrabBags where

-- 1 -- 
-- all equivalent
mTh_a x y z = x * y * z

mTh_b x y = \z -> x * y * z

mTh_c x = \y -> \z -> x * y * z

mTh_d = \x -> \y -> \z -> x * y * z




-- anonymous lambda syntax

-- a --
addOneIfOdd' n = case odd n of 
    True -> f n 
    False -> n 
    where f = \n -> n + 1


-- b --
addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5

-- c --
-- Reverse : rewrite without the anon syntax

mflip f = \x -> \y -> f y x

mflip' f x y = f y x 













