{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.List as List
import Data.Set as Set

sieve :: Int -> [Int]
sieve = undefined

getFirstUniqueElementInList :: (Eq a) => [a] -> [a] -> a
-- \\ is the list difference operator apparently
getFirstUniqueElementInList x y = head $ x List.\\ y
prop_getFirstUniqueElementInList
    = getFirstUniqueElementInList [2,7,11,23] [2,7,23] == 11

squareGELimit :: Int -> Int -> Bool
squareGELimit x y = x*x >= y
prop_squareGELimitTrueApp  = True == squareGELimit 7 30
prop_squareGELimitFalseApp = False == squareGELimit 7 50

dropEveryNth :: Int -> [a] -> [a]
dropEveryNth n xs = case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : dropEveryNth n ys
-- in the first 29 natural numbers, the expected output would be
-- [5,10,15,20,25]
prop_dropNthApp :: Bool
prop_dropNthApp = [5,10,15,20,25] == dropEveryNth 5 (take 29 ([1..]))

tests :: [(String, Bool)]
tests = 
    [("it takes every nth element from a list", prop_dropNthApp),
     ("it is true for the square of seven greater than / equals 30", prop_squareGELimitTrueApp),
     ("it is false for the square of seven greater than / equals 50", prop_squareGELimitFalseApp),
     ("it gets the first unique element from two lists", prop_getFirstUniqueElementInList)]

testResult :: Bool
testResult = all (\x -> snd x == True) tests
