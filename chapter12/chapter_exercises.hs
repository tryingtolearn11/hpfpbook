-- chapter_exercises.hs --

module ChapterExercises where


-- String Processing --

-- 1 --
-- helper function: notThe
notThe :: String -> Maybe String
notThe xs 
  | xs == "the" = Nothing
  | otherwise   = Just xs

replaceThe [] = []
replaceThe xs = unwords $ map f (words xs)
    where f s = case notThe s of
                          Just s -> s
                          Nothing -> "a"



-- 2 --

initialvowel :: String -> Bool
initialvowel xs = (head xs) `elem` ("aeiou")


-- countTheBeforeVowel :: String -> Integer
