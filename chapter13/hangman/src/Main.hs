module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


type WordList = [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw) 
        where gameLength w = let l = length (w :: String) in l >= minWordLength && l < maxWordLength


allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)














main :: IO ()
main = do
  putStrLn "hello world"