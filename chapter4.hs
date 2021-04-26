-- chapter4.hs

module Chapter4 where

-- 8. Write a function that tells you whether or not a given String (or list) is a
-- palindrome. Use predefined function reverse.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == reverse(x))

-- 9. Write a function to return the absolute value of a number using
-- if-then-else
myAbs :: Integer -> Integer
myAbs x =
    if negative 
       then (x * (negate 1))
    else x
    
    where negative = 
                (x < 0)



f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ( (snd x, snd y), (fst x, fst y) )
        

-- correcting syntax

func xs = w + 1 
    where w = length xs
