-- twelve_point_two.hs --

module TwelvePointTwo where

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
    if (even n) then Just (n + 2) else Nothing


-- Type Aliases
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show


data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

-- mkperson is what we call a smart constructor
-- since it allows us to construct values of a type ONLY when 
-- they meet the criteria. Otherwise it returns an explicit signal
-- when the criteria is not met
    {-
mkPerson :: Name  -> Age -> Maybe Person
mkPerson name age
  | name     /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing
  -}

-- In this updated version, if we succeed we get a Person, otherwise
-- PersonInvalid 
mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow


-- We use 'Left' as our invalid/error constructor since its conventional. 
-- You normally want to apply functions and map over the case that DOESN'T
-- stop your program (that is, NOT the error case), it has become convention
-- that the 'Left' of 'Either' is used for whatever case is going to cause the
-- work to stop.


