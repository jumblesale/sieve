{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.List as List
import Data.Map.Strict as Map hiding (map)
import Data.Set as Set hiding (map)

sieve :: Int -> [Int]
sieve = undefined

-- generate multiples of a number starting at the number's square
generateMultiples :: Int -> Int -> [Int]
generateMultiples limit n = takeWhile (<= limit) $ map (*n) [n..]
prop_generateMultiplesOf2 = [4,6,8,10,12] == generateMultiples 12 2
prop_generateMultiplesOf5 = [25,30,35,40] == generateMultiples 40 5

nonPrimesFromMap :: Map Int Bool -> [Int]
nonPrimesFromMap = Map.keys . Map.filter(== False)

nextCandidate :: Set Int -> Map Int Bool -> Int
nextCandidate visited candidates = head $ nonPrimesFromMap candidates List.\\ Set.toList visited
prop_nextCandidateFirstVisitedGives2 = 2 == nextCandidate Set.empty (Map.fromList [(2,False)])
prop_nextCandidateAfter2And3Gives4   =
    4 == nextCandidate (Set.fromList [2,3]) (Map.fromList [(2,True),(3,True),(4,False)])

tests :: [(String, Bool)]
tests = 
    [("it gets the first candidate as 2", prop_nextCandidateFirstVisitedGives2),
     ("it gets the next candidate after 2 and 3 as 4", prop_nextCandidateAfter2And3Gives4),
     ("it generates mutliples of 2", prop_generateMultiplesOf2),
     ("it generates multiples of 5", prop_generateMultiplesOf5)
    ]

testResult :: Bool
testResult = all (\x -> snd x == True) tests

testCandidates :: Map Int Bool
testCandidates = Map.fromList $ zip ([2,3..30]) (repeat False)

-- the first 30 primes
testExpectedResult :: [Int]
testExpectedResult = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
