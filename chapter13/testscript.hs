module TestScript where

import Control.Monad (forever)
import Data.Char (toLower)
import System.Exit (exitSuccess)
import System.IO



data Puzzle = Puzzle String [Maybe Char] [Char] deriving Show

fpuzzle :: String -> Puzzle
fpuzzle word = Puzzle (word) (f word) []

f :: [a1] -> [Maybe a2]
f [] = []
f xs = go (length xs - 1)
    where go x
            | x == 0 = []
            | otherwise = Nothing : go (x - 1)


charInWord :: Puzzle -> Char -> Bool 
charInWord (Puzzle xs _ _) x = elem x xs

alreadyguess :: Puzzle -> Char -> Bool
alreadyguess (Puzzle _ _ xs) x = elem x xs




-- 2 --
palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case ((filtered $ line1) == reverse (filtered line1)) of
      True -> putStrLn "It's a palindrome!"
      False -> do
          putStrLn "Nope!"
          exitSuccess

    where filtered l = map toLower (concat $ words l) 


-- 4 --
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)


mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknown $ "Name was: " ++
                            show name ++ "Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStrLn "Please Enter Name: "
    name <- getLine
    putStrLn "Please Enter Age: "
    input2 <- getLine
    let age = (read input2 :: Integer)
    case (mkPerson name age) of
      Left NameEmpty -> 
          putStrLn "Error in Person Data: Name empty" 
      Left AgeTooLow -> 
          putStrLn "Error in Person Data: Age too Low"
      Left (PersonInvalidUnknown err) -> 
          putStrLn $ "Error in Person Data: " ++ err
      Right person -> 
          putStrLn $ "Yay! Successfuly got a Person:" ++ show person
















