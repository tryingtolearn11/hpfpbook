module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO()
sayHello = putStrLn "hello!"

-- Generators
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b)
          => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]


-- Equal Probablity
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]


-- What QuickCheck does so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing), (3, return (Just a)) ]

-- Using QuickCheck without hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
---------------------------------



dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d     = (count, n)
            | otherwise = go (n - d) d (count + 1)


rMult :: Integral a => a -> a -> a
rMult x y = go x y 0 0
    where go x y count sum
            | count == y = sum
            | otherwise  = go x y (count + 1) (sum + x)


main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "10 multiplied by 2 is 20" $ do
            rMult 10 2 `shouldBe` 20
        it "22 multiplied by 5 is 110" $ do
            rMult 22 5 `shouldBe` 110
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

