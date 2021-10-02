module TestScript where



data Puzzle = Puzzle String [Maybe Char] [Char] deriving Show

fpuzzle :: String -> Puzzle
fpuzzle word = Puzzle (word) (f word) []

f :: [a1] -> [Maybe a2]
f [] = []
f xs = go (length xs - 1)
    where go x
            | x == 0 = Nothing  : []
            | otherwise = Nothing : go (x - 1)


charInWord :: Puzzle -> Char -> Bool 
charInWord (Puzzle xs _ _) x = elem x xs

alreadyguess :: Puzzle -> Char -> Bool
alreadyguess (Puzzle _ _ xs) x = elem x xs
