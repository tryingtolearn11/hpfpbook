module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

-- Now we set up generators for ensuring that the random
-- values QuickCheck uses to test our program are sensible
-- for our Morse code program


allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse




prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen
    (\c -> (( charToMorse c) >>= morseToChar) == Just c)




main :: IO ()
main = quickCheck prop_thereAndBackAgain























