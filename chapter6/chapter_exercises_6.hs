-- chapter_exercises_6.hs

module ChapterExercises where

import Data.List (sort)

-- Does it typecheck?

-- 1 --
-- Original : data Person = Person Bool
data Person = Person Bool deriving Show

printPerson :: Person -> IO  ()
printPerson person = putStrLn (show person)

-- 2 --
-- Original : data Mood = Blah | Woot deriving Show
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

-- 4 --
type Subject = String
type Verb    = String
type Object  = String

data Sentence = 
    Sentence Subject Verb Object deriving (Eq, Show)


-- Original : s1 = Sentence "dogs" "drool"
s1 = Sentence "dogs" "drool" "lol"
s2 = Sentence "Julie" "loves" "dogs"


-- Datatype Declarations

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1 --
-- phew = Papu "chases" True : Will not work
phew = Papu x y where
    x = Rocks "chases"
    y = Yeah True

-- 2 --
-- This will work
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3 --
-- This will work
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- 4 --
-- This will not work
{-
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
-}

-- Match the types
-- 1 --
i :: Num a => a
i = 1

-- 2 --
f :: Float
f = 1.0

-- 3 --
-- h :: Float works also
h :: Fractional a => a
h = 1.0

-- 4 --
g :: RealFrac a => a
g = 1.0

-- 5 --
-- freud :: a -> a
-- Both methods work since Ord already assumes Eq
freud :: Ord a => a -> a
freud x = x

-- 6 --
-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- 7 --
myX = 1 :: Int

-- sigmund :: a -> a -- type a is non concrete
sigmund :: Int -> Int
sigmund x = myX

-- 8 --
myZ = 1 :: Int
--sigmund' :: Num a => a -> a -- same as 7
sigmund' :: Int -> Int
sigmund' x = myZ

-- 9 --
--jung :: Ord a => [a] -> a -- Both works fine
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10 --
--young :: [Char] -> Char -- Both works fine
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11 --
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a -- Will not work
signifier xs = head (mySort xs)










-- TKD --
-- 1 --
-- TODO: Figure why these work! 
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y


-- 2 --
arith :: Num b => (a -> b) -> Integer -> a -> b















































