-- types.hs --

module Types where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vechicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir


-- 2 --
isCar :: Vechicle -> Bool
isCar (Car _ _) = True
isCar (_)       = False

isPlane :: Vechicle -> Bool
isPlane (Plane _ ) = True
isPlane (_) = False

areCars :: [Vechicle] -> [Bool]
areCars = map isCar

