-- fearful_symmetry.hs --

module FearfulSymmetry where

import Debug.Trace

stringSplit :: [Char] -> [[Char]]
stringSplit [] = []
stringSplit (' ':x) = trace ("xs = " ++ show x ++ "\n") (stringSplit x)
stringSplit x = trace("x = " ++ show x ++ " "++ "\n") (takeWhile (/= ' ') x : (stringSplit (dropWhile (/= ' ') x)))

