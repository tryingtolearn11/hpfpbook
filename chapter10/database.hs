-- database.hs --

module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase =
    [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]

-- 1 --
filterDbdate :: [DatabaseItem] -> [UTCTime]
filterDbdate xs = foldr dbtimes [] xs
    where dbtimes (DbDate x) xs = x : xs
          dbtimes _ xs          = xs


-- 2 --
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr dbNums [] xs
    where dbNums (DbNumber x) xs = x : xs
          dbNums _ xs            = xs


-- 3 --
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbdate


-- 4 --
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5 --
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral $ (sumDb xs)) / (fromIntegral $ (length . filterDbNumber) xs)






























