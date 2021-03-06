-- cipher.hs --

module Cipher where

import Data.Char

right_shift_cipher :: String -> Int -> String
right_shift_cipher [] _ = []
right_shift_cipher (' ':xs) (k) = right_shift_cipher xs k
right_shift_cipher (x : xs) (k)
  | (ord x + k) > (ord 'z')  = chr (((ord x + k) `mod` 122) + 96) : right_shift_cipher xs k
  | (ord x + k) == (ord 'z') = chr (ord x + k) : right_shift_cipher xs k
  | otherwise                = chr ((ord x + k) `mod` 122) : right_shift_cipher xs k



right_shift_decipher :: String -> Int -> String
right_shift_decipher [] _ = []
right_shift_decipher (' ': xs) k = right_shift_decipher xs k
right_shift_decipher (x : xs) (k)
  | (ord x - k) < (ord 'a') = chr ((122) - (96 - (((ord x - k) `mod` 122)))) : right_shift_decipher xs k
  | otherwise               = chr (( ord x - k) `mod` 122) : right_shift_decipher xs k
