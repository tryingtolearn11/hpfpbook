-- derivingInstances.hs

module DerivingInstances where

data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving Show

-- Default Ord Instance: Values to the left are LESS THAN values placed to the right
-- deriving (Ord, Show)

-- Write own Ord Instance to make Friday greatest day
instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _   = GT
    compare _ Fri   = LT
    compare _ _     = EQ






data Date =
    Date DayOfWeek Int


instance Eq DayOfWeek where
    (==) Mon Mon   = True
    (==) Tue Tue   = True
    (==) Weds Weds = True
    (==) Thu Thu   = True
    (==) Fri Fri   = True
    (==) Sat Sat   = True
    (==) Sun Sun   = True
    (==) _ _       = False


instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
             weekday == weekday' && dayOfMonth == dayOfMonth'


data Identity a =
    Identity a


instance Eq a => Eq (Identity a) where
    (==) (Identity v) ( Identity v') = v == v'


-- Exercises 6.5 --

-- 1 --
data TisAnInteger = TisAn Integer

instance Eq (TisAnInteger) where
    (==) (TisAn x) (TisAn x') = x == x'

-- 2 --
data TwoIntegers = Two Integer Integer

instance Eq (TwoIntegers) where
    (==) (Two x y) (Two x' y') = x == x' && y == y'

-- 3 --
data StringOrInt = TisAnInt Int | TisAString String

instance Eq (StringOrInt) where
    (==) (TisAnInt x) (TisAnInt x')     = x == x'
    (==) (TisAString y) (TisAString y') = y == y'
    (==) _ _                            = False
 
-- 4 --
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

-- 5 --
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'


-- 6 --
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne y) (ThatOne y') = y == y'
    (==) _ _                      = False


-- 7 --
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x')     = x == x'
    (==) (Goodbye y) (Goodbye y') = y == y'
    (==) _ _                      = False



