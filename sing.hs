-- sing.hs

module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"


sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"


-- 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f $ x

-- 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w $ q $ a

-- 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (a, b) = (xz a, yz b)

-- 4
{-
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
-}

