-- chapter3.hs

module Reverse where

str :: String
str = "Curry is awesome"

str2 :: String
str2 = "Curry is awesome!"

-- concatenate
concatentateString :: String -> String -> String
concatentateString str x = concat [str, x]

-- indexing
index = str !! x
    where
        x = 4

-- drop
dropping = drop x str2
    where x = 9

-- Much cleaner ---------------------------------------------------------------

-- Write a function of type string -> char which returns the 3rd char in string
thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = str !! x

-- Write rvrs to reverse the string using only take and drop for str already
-- made

rvrs :: String -> String
rvrs str = concat [(drop 9 $ str), (drop 5 $ (take 9 $ str)), (take 5 $ str)]

main :: IO ()
main = print $ rvrs $ str
