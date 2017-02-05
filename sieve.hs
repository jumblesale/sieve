{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

allEvens :: [Int]
allEvens = [2, 4..]

dropEveryNth :: Int -> [Int] -> [Int]
dropEveryNth n xs = case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : dropEveryNth n ys
-- in the first 29 natural numbers, the expected output would be
-- [5,10,15,20,25]
prop_dropNthApp :: Bool
prop_dropNthApp = [5,10,15,20,25] == dropEveryNth 5 (take 29 ([1..]))

tests :: [(String, Bool)]
tests = [("it takes every nth element from a list", prop_dropNthApp)]
