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

data Puzzle = Puzzle String [Maybe Char] [Char]
--                   [1]     [2]          [3]
-- [1]: the word to guess
-- [2]: characters filled in so far
-- [3]: letters guessed so far

-- Instance for the typeclass Show for out datatype Puzzle.
instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

-- filters word list to fit the criteria length defined i.e: shorter word list
gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw) 
        where gameLength w = let l = length (w :: String) in l >= minWordLength && l < maxWordLength


allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

-- pull random word from gameWords list

-- randomWord - generates a random index number (based on length of word list
-- wl, and then selects the member of list that is at that indexed position.
randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO(0, (length wl - 1))
    return $ wl !! randomIndex



-- randomWord' - binds the gameWords list to randomWord function so the random
-- word we get is from that list ( '>>=' operator known as "bind")
randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- Write a function that takes puzzle word and converts it into a list of
-- Nothing.
freshPuzzle :: String -> Puzzle
freshPuzzle = undefined




main :: IO ()
main = do
  putStrLn "hello world"
