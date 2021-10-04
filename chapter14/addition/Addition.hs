module Addition where

import Test.Hspec

sayHello :: IO()
sayHello = putStrLn "hello!"


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

