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



