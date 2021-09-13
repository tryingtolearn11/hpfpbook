-- types.hs --

module Types where
{-

Data Declaration Breakdown

    data Bool = False | True
     [1] [2] [3] [4] [5] [6]  
    
    
    data [] a = [] | a : [a]
         [7]    [8]   [9]


[1] - keyword 'data' signals a Data Declaration / declaration of datatype
[2] - Type Construtor (w/ no arguments)
[3] - = Equal Sign divides the type constructor from its data constructors
[4] - Data Constructor (one that takes no arguments is a 'nullary constructor'
[5] - The Pipe denotes a sum type (logical disjunction OR) 
[6] - nullary constructor for the value True

[7] - Type Constructor with an argument. An empty list has to be applied 
      to an argument in order to become a list of something. Here the argument
      is a polymorphic type variable, so the list's argument can be of different
      types.
[8] - Data Constructor for empty list
[9] - Data Constructor that takes 2 arguments: a and [a]

-}
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

-- 3 --
getManu :: Vechicle -> Manufacturer
getManu (Car x _) = x
