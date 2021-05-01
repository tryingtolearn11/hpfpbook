-- matchingTuples1.hs --

module TupleFunctions where

addEmup2 :: Num a => (a, a) -> a
addEmup2 (x, y) = x + y

-- alternatively --
addEmup2Alt :: Num a => (a, a) -> a
addEmup2Alt tup = (fst tup) + (snd tup)


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x



-- Exercises : 2 --

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
