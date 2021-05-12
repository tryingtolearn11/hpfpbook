-- exercises_folds.hs --

module ExercisesFolds where


-- a --
fa = foldr (++) "HAHAHA" ["woot", "WOOT", "woot"]

-- b --
fb = foldr max ' ' "fear is the little death"

-- c --
fc = foldr (&&) True [False, True]

-- d --
fd = foldr (||) True [False, True]

-- e --
-- fe = foldl (concat . show) '' [1..5]

-- g --
fg = foldl const '0' "tacos"

-- h --
fh = foldl (flip const) '0' "burritos"

-- i --
fi = foldl (flip const) '0' ['1', '2', '3', '4', '5']


