-- penguins.hs --

module Penguins where

data WherePenguinsLive =
    Galapago
    | Antartica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)


data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _           = False


gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives
    
      
