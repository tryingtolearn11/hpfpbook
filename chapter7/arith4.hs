-- arith4.hs --

module Arith4 where

{-
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)
-}

-- 6 --
roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

-- 5 --
-- Point Free Version
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show


main = do
    print (roundTrip 4 :: Int)
    print (roundTrip 'x' :: Char)
    print (id 4)
