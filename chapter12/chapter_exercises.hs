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
-- If we found "the", check the element ahead for initial vowel
-- If no "the" found, move on and recurse

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = g (words xs)
    where g [] = 0
          g ("the" : y : ys)   
            | (head y) `elem` "aeiou" = g (ys) + 1
            | otherwise               = g (y:ys)
          g (y : ys) = g (ys) 



