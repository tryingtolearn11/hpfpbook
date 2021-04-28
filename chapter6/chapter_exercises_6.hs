-- chapter_exercises_6.hs

module ChapterExercises where


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

