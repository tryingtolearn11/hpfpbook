-- fearful_symmetry.hs --

module FearfulSymmetry where

import Debug.Trace


stringSplit :: [Char] -> [[Char]]
stringSplit [] = []
stringSplit (' ':x) = stringSplit x
stringSplit x = takeWhile (/= ' ') x : (stringSplit (dropWhile (/= ' ') x))


stringSplit' :: [Char] -> [[Char]]
stringSplit' [] = []
stringSplit' (' ':x) = trace ("xs = " ++ show x ++ "\n") (stringSplit' x)
stringSplit' x = trace("x = " ++ show x ++ " "++ "\n") (takeWhile (/= ' ') x : (stringSplit' (dropWhile (/= ' ') x)))


-- 2 --
first = "Tyger Tyger, buring bright\n"
second = "In the forests of the night\n"
third = "What immortal hand or eye\n"
fourth = "Could frame thy fearful\
         \ symmetry?"

sentences = first ++ second ++ third ++ fourth
stringSplitNewLine :: String -> [String]
stringSplitNewLine [] = []
stringSplitNewLine ('\n':x) = stringSplitNewLine x
stringSplitNewLine x = takeWhile (/= '\n') x : (stringSplitNewLine (dropWhile (/= '\n') x))
