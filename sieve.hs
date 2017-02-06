{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.List as List
import Data.Map.Strict as Map hiding (map)
import Data.Set as Set hiding (map)

sieve :: Int -> [Int]
sieve = undefined


markMultiplesAsNonPrime :: Int -> Int -> Map Int Bool -> Map Int Bool
markMultiplesAsNonPrime limit n m = markIndexesAsNonPrime m (generateMultiples limit n)
prop_markMultiplesAsNonPrimeMarksMultiplesOf2AsNonPrime =
    Map.fromList [(2, True), (3, True), (4, False), (5, True), (6, False)] ==
    markMultiplesAsNonPrime 6 2 (Map.fromList $ zip ([2,3..6]) (repeat True))

-- this should be a fold?
markIndexesAsNonPrime :: Map Int Bool -> [Int] -> Map Int Bool
markIndexesAsNonPrime m [] = m
markIndexesAsNonPrime m (x:xs) = markIndexesAsNonPrime (markIndexAsNonPrime x m) xs
prop_markIndexesAsNonPrimeMarksIndexesAsNonPrime =
    Map.fromList [(2, False), (3, True), (4, False)] ==
    markIndexesAsNonPrime (Map.fromList [(2, True), (3, True), (4, True)]) [2, 4]

markIndexAsNonPrime :: Int -> Map Int Bool -> Map Int Bool
markIndexAsNonPrime index m = Map.insert index False m
prop_markIndexAsNonPrimeSetsIndexToFalse =
    (Map.fromList [(2, True), (3, False)]) ==
    markIndexAsNonPrime 3 (Map.fromList [(2, True),(3, True)])

-- generate multiples of a number starting at the number's square
generateMultiples :: Int -> Int -> [Int]
generateMultiples limit n = takeWhile (<= limit) $ map (*n) [n..]
prop_generateMultiplesOf2 = [4,6,8,10,12] == generateMultiples 12 2
prop_generateMultiplesOf5 = [25,30,35,40] == generateMultiples 40 5

nonPrimesFromMap :: Map Int Bool -> [Int]
nonPrimesFromMap = Map.keys . Map.filter(== False)

candidatesFromMap :: Map Int Bool -> [Int]
candidatesFromMap = Map.keys . Map.filter(== True)

nextCandidate :: Set Int -> Map Int Bool -> Int
nextCandidate visited candidates = head $ candidatesFromMap candidates List.\\ Set.toList visited
prop_nextCandidateFirstVisitedGives2 = 2 == nextCandidate Set.empty (Map.fromList [(2,True)])
prop_nextCandidateAfter2And3Gives4   =
    4 == nextCandidate (Set.fromList [2,3]) (Map.fromList [(2,False),(3,False),(4,True)])

tests :: [(String, Bool)]
tests = 
    [("it gets the first candidate as 2", prop_nextCandidateFirstVisitedGives2),
     ("it gets the next candidate after 2 and 3 as 4", prop_nextCandidateAfter2And3Gives4),
     ("it generates mutliples of 2", prop_generateMultiplesOf2),
     ("it generates multiples of 5", prop_generateMultiplesOf5),
     ("it marks a single index as non-prime", prop_markIndexAsNonPrimeSetsIndexToFalse),
     ("it marks indexes as non-prime", prop_markIndexesAsNonPrimeMarksIndexesAsNonPrime),
     ("it marks multiples of a number as non-primes", prop_markMultiplesAsNonPrimeMarksMultiplesOf2AsNonPrime)
    ]

testResult :: Bool
testResult = all (\x -> snd x == True) tests

testCandidates :: Map Int Bool
testCandidates = Map.fromList $ zip ([2,3..30]) (repeat True)

-- the first 10 primes under 30
testExpectedResult :: [Int]
testExpectedResult = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
