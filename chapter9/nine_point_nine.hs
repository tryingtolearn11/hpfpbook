-- nine_point_nine.hs --


module NinePointNine where

import Data.Bool

itIsMystery xs =
    map (\x -> elem x "aeiou") xs



f = map (\x -> bool (x) (-x) (x == 3))



