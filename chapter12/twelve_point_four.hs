-- twelve_point_four.hs --

module TwelvePointFout where


-- Example of a type that has a type constructor
-- rather than a type constant.

data Example a = Blah | RoofGoats | Woot a

-- Example is a type constructor because it takes a type argument a
-- which is used witht he Woot data constructor.


-- Safe version of 'tail' function
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

