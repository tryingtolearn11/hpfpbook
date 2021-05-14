-- chapter_exercises.hs --

module ChapterExercises where

-- 1 --
f [] _ = []
f (_:[]) _ = []
f (x : xy) z = (x, z, (head xy)) : (head xy, z, x) : f (xy) z

