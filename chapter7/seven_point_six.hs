-- seven_point_six.hs --

module SevenPointSix where

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)


reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
    putStrLn $ show e ++ " is the boss of " ++ show e'

{-
employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
    case compare e e' of
      GT -> reportBoss e e'
      EQ -> putStrLn "Neither employee \\ is the boss"
      LT -> (flip reportBoss) e e'
-}



codersRuleCEOsDrool :: Employee
                    -> Employee
                    -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e' = compare e e'




-- more flexible --

employeeRank :: (Employee
             -> Employee
             -> Ordering)
             -> Employee
             -> Employee
             -> IO ()
employeeRank f e e' =
    case f e e' of
      GT -> reportBoss e e'
      EQ -> putStrLn "Neither employee is the boss"
      LT -> (flip reportBoss) e e'




-- Artful Dodgy --

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2


-- Guard Duty --

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100


pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False


numbers :: (Ord a, Num a, Num p) => a -> p
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1













