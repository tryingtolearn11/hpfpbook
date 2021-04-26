
module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x =
    x + woot + topLevelValue
    where woot :: Integer
          woot = 10


topLevelValue :: Integer
topLevelValue = 5




{-
sayHello :: String -> IO ()
sayHello x = 
    putStrLn("Hello, " ++ x ++ "!")

triple x = x * 3

ex_two y = (y ^ 2) * 3.14
-}

{-
area d = pi * (r * r)
r = d / 2
d = 10
-}




area d = (*pi) $ (r * r)
    where r = d / 2

{-
area d = pi * (r * r)
    where r = d / 2
-}
