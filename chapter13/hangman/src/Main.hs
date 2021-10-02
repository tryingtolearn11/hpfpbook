module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


newtype WordList = WordList [String] deriving (Eq, Show)

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
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw) 
        where gameLength w = let l = length (w :: String) in l >= minWordLength && l < maxWordLength


allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

-- pull random word from gameWords list

-- randomWord - generates a random index number (based on length of word list
-- wl, and then selects the member of list that is at that indexed position.
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO(0, (length wl) - 1)
    return $ wl !! randomIndex



-- randomWord' - binds the gameWords list to randomWord function so the random
-- word we get is from that list ( '>>=' operator known as "bind")
randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- Write a function that takes puzzle word and converts it into a list of
-- Nothing.
freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle (word) (f word) []


f :: [a1] -> [Maybe a2]
f [] = []
f xs = go (length xs - 1)
    where go x
            | x == 0 = []
            | otherwise = Nothing : go (x - 1)



-- Now a function that looks at Puzzle String and determines whether the
-- character guessed is an element of that string
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle xs _ _) x = elem x xs


-- Now a function that checks only to see if it is an element of the guessed
-- list
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ xs) x = elem x xs


-- check if character is correctly guessed, if incorrect then Nothing, else
-- Just 'x'
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar x = case x of
                       Just x -> x
                       Nothing -> '_'

-- if guesses correctly we wrap the char in a Maybe otherwise we just return
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar ( c : s )
        where zipper guessed wordChar guessChar = 
               if wordChar == guessed 
               then Just wordChar 
               else guessChar 
              newFilledInSoFar = zipWith (zipper c) word filledInSoFar


-- Case expressions to handle guesses
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
          putStrLn "You already guessed that\
                   \ character, pick something else!"
          return puzzle
      (True, _) -> do
          putStrLn "This character was in the\
                   \ word, filling in the word accordingly"
          return (fillInCharacter puzzle guess)
      (False, _) -> do
          putStrLn "This character wasn't in\
                   \ the word, try again. "
          return (fillInCharacter puzzle guess)



-- When to stop? After a certain number of guesses
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (length guessed) > 10 then
                            do putStrLn "You Lose!"
                               putStrLn $ "The word was : " ++ wordToGuess
                               exitSuccess
                            else return ()



-- Since the puzzle word is a list of Maybe values, when each character is
-- represented by a Just Char instead of a Nothing, you win and exit
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar then
                                do putStrLn "You Win!"
                                   exitSuccess
                                else return ()



runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStrLn "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must be a single character"





main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle




