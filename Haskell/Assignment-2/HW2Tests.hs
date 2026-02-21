{-Haskell is available for Windows, Mac, and Linux. Here's the download page: https://www.haskell.org/downloads/.
We will be using the HUnit unit testing package in CptS 355.

Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "runTestTT tests" at the Haskell prompt.  -}

{- CptS 355 - Spring 2026 Assignment 2 Tests
   Name: Camille Orego
   
   This file includes the provided sample tests and additional edge case tests
   for each problem (at least 2 new tests per subproblem).
   
   To run: runTestTT tests  (or use the shortcut: run)

   AI generated testing.  I used Claude Sonnet 4.6 to generate additional test cases for each problem, including edge cases. 
   I provided the problem description and sample tests as context for the AI, and then asked it to generate new test cases 
   that would cover different scenarios and edge cases.
-}

module HW2Tests
    where

import Test.HUnit
import Data.Char
import HW2

-- =============================================================================
-- PROBLEM 1a: merge2
-- =============================================================================

-- Provided sample tests
p1a_test1 = TestCase (assertEqual "merge2 [2,5,6,8,9] [1,3,4,5,7,8,10]"
    [1,2,3,4,5,5,6,7,8,8,9,10]
    (merge2 [2,5,6,8,9] [1,3,4,5,7,8,10]))

-- Additional tests
p1a_test2 = TestCase (assertEqual "merge2 empty first list"
    [1,2,3]
    (merge2 [] [1,2,3]))

p1a_test3 = TestCase (assertEqual "merge2 empty second list"
    [1,2,3]
    (merge2 [1,2,3] []))

p1a_test4 = TestCase (assertEqual "merge2 with all duplicates"
    [1,1,2,2,3,3]
    (merge2 [1,2,3] [1,2,3]))

p1a_test5 = TestCase (assertEqual "merge2 with negatives"
    [-5,-3,-1,0,2,4]
    (merge2 [-5,-1,2] [-3,0,4]))

-- =============================================================================
-- PROBLEM 1b: merge2Tail
-- =============================================================================

-- Provided sample test
p1b_test1 = TestCase (assertEqual "merge2Tail [2,5,6,8,9] [1,3,4,5,7,8,10]"
    [1,2,3,4,5,5,6,7,8,8,9,10]
    (merge2Tail [2,5,6,8,9] [1,3,4,5,7,8,10]))

-- Additional tests (should match merge2 output exactly)
p1b_test2 = TestCase (assertEqual "merge2Tail both empty lists"
    []
    (merge2Tail [] ([] :: [Int])))

p1b_test3 = TestCase (assertEqual "merge2Tail single element each"
    [1,2]
    (merge2Tail [1] [2]))

p1b_test4 = TestCase (assertEqual "merge2Tail matches merge2 output on negatives"
    (merge2 [-5,-1,2] [-3,0,4])
    (merge2Tail [-5,-1,2] [-3,0,4]))

-- =============================================================================
-- PROBLEM 1c: mergeN
-- =============================================================================

-- Provided sample test
p1c_test1 = TestCase (assertEqual "mergeN [[3,4],[-3,-2,-1],[1,2,5,8,9]]"
    [-3,-2,-1,1,2,3,4,5,8,9]
    (mergeN [[3,4],[-3,-2,-1],[1,2,5,8,9]]))

-- Additional tests
p1c_test2 = TestCase (assertEqual "mergeN empty list of lists"
    []
    (mergeN ([] :: [[Int]])))

p1c_test3 = TestCase (assertEqual "mergeN single sublist"
    [1,2,3]
    (mergeN [[1,2,3]]))

p1c_test4 = TestCase (assertEqual "mergeN with an empty sublist"
    [1,2,3,4]
    (mergeN [[1,3],[],[2,4]]))

-- =============================================================================
-- PROBLEM 2a: getInRange
-- =============================================================================

-- Provided sample tests
p2a_test1 = TestCase (assertEqual "getInRange (-5) 5 [10,5,0,1,2,-5,-10]"
    [0,1,2]
    (getInRange (-5) 5 [10,5,0,1,2,-5,-10]))

p2a_test2 = TestCase (assertEqual "getInRange (-1) 1 [-2,2,3,4,5]"
    []
    (getInRange (-1) 1 [-2,2,3,4,5]))

-- Additional tests
p2a_test3 = TestCase (assertEqual "getInRange boundary values excluded"
    [4,5,6,7,8,9]
    (getInRange 3 10 [1,2,3,4,5,6,7,8,9,10,11]))

p2a_test4 = TestCase (assertEqual "getInRange empty input list"
    []
    (getInRange 0 10 []))

p2a_test5 = TestCase (assertEqual "getInRange with strings"
    ["b","c"]
    (getInRange "a" "d" ["a","b","c","d","e"]))

-- =============================================================================
-- PROBLEM 2b: countInRange
-- =============================================================================

-- Provided sample tests
p2b_test1 = TestCase (assertEqual "countInRange 3 10 [[1,2,3,4],[5,6,7,8,9],[10,11]]"
    6
    (countInRange 3 10 [[1,2,3,4],[5,6,7,8,9],[10,11]]))

p2b_test2 = TestCase (assertEqual "countInRange (-5) 5 [[-10,-5,-4],[0,4,5],[],[10]]"
    3
    (countInRange (-5) 5 [[-10,-5,-4],[0,4,5],[],[10]]))

-- Additional tests
p2b_test3 = TestCase (assertEqual "countInRange empty nested list"
    0
    (countInRange 0 10 []))

p2b_test4 = TestCase (assertEqual "countInRange all empty sublists"
    0
    (countInRange 0 10 [[],[],[]]))

p2b_test5 = TestCase (assertEqual "countInRange none qualify (boundaries excluded)"
    0
    (countInRange 1 5 [[1,5],[1],[5],[]]))

-- =============================================================================
-- PROBLEM 3a: addLengths
-- =============================================================================

-- Provided sample tests
p3a_test1 = TestCase (assertEqual "addLengths (FOOT 2) (INCH 5)"
    (INCH 29)
    (addLengths (FOOT 2) (INCH 5)))

p3a_test2 = TestCase (assertEqual "addLengths (YARD 3) (INCH (-3))"
    (INCH 105)
    (addLengths (YARD 3) (INCH (-3))))

-- Additional tests
p3a_test3 = TestCase (assertEqual "addLengths YARD + YARD"
    (INCH 72)
    (addLengths (YARD 1) (YARD 1)))

p3a_test4 = TestCase (assertEqual "addLengths FOOT + FOOT"
    (INCH 96)
    (addLengths (FOOT 4) (FOOT 4)))

p3a_test5 = TestCase (assertEqual "addLengths YARD + FOOT"
    (INCH 48)
    (addLengths (YARD 1) (FOOT 1)))

-- =============================================================================
-- PROBLEM 3b: addAllLengths
-- =============================================================================

-- Provided sample test
p3b_test1 = TestCase (assertEqual "addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]"
    (INCH 262)
    (addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]))

-- Additional tests
p3b_test2 = TestCase (assertEqual "addAllLengths empty list"
    (INCH 0)
    (addAllLengths []))

p3b_test3 = TestCase (assertEqual "addAllLengths with empty sublists"
    (INCH 24)
    (addAllLengths [[], [FOOT 2], []]))

p3b_test4 = TestCase (assertEqual "addAllLengths [[FOOT 2], [FOOT 2, INCH 2],[]]"
    (INCH 50)
    (addAllLengths [[FOOT 2], [FOOT 2, INCH 2],[]]))

-- =============================================================================
-- PROBLEM 4a: sumTree
-- =============================================================================

-- Sample tree from assignment
t1 = NODE 1
         (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6))
         (NODE 7 (LEAF 8) (LEAF 9))

-- Provided sample test
p4a_test1 = TestCase (assertEqual ("sumTree " ++ (show t1))
    32
    (sumTree t1))

-- Additional tests
p4a_test2 = TestCase (assertEqual "sumTree single LEAF"
    42
    (sumTree (LEAF 42)))

p4a_test3 = TestCase (assertEqual "sumTree myTree (leaves: 2,3,4,5,6 = 20)"
    20
    (sumTree myTree))

p4a_test4 = TestCase (assertEqual "sumTree symmetric tree"
    10
    (sumTree (NODE 0 (NODE 0 (LEAF 1) (LEAF 2)) (NODE 0 (LEAF 3) (LEAF 4)))))

-- =============================================================================
-- PROBLEM 4b: createSumTree
-- =============================================================================

-- Expected output for t1
t1_output = NODE 32
    (NODE 15 (NODE 9 (LEAF 4) (LEAF 5)) (LEAF 6))
    (NODE 17 (LEAF 8) (LEAF 9))

-- Provided sample test
p4b_test1 = TestCase (assertEqual ("createSumTree " ++ (show t1))
    t1_output
    (createSumTree t1))

-- Additional tests
p4b_test2 = TestCase (assertEqual "createSumTree single LEAF unchanged"
    (LEAF 7)
    (createSumTree (LEAF 7)))

p4b_test3 = TestCase (assertEqual "createSumTree myTree"
    (NODE 20
        (NODE 9 (LEAF 2) (NODE 7 (LEAF 3) (LEAF 4)))
        (NODE 11 (LEAF 5) (LEAF 6)))
    (createSumTree myTree))

p4b_test4 = TestCase (assertEqual "createSumTree simple two-leaf tree"
    (NODE 3 (LEAF 1) (LEAF 2))
    (createSumTree (NODE 0 (LEAF 1) (LEAF 2))))

-- =============================================================================
-- PROBLEM 5: foldListTree
-- =============================================================================

-- Sample ListTree values from assignment
t4 = ListNODE
    [ ListNODE [ ListLEAF [1,2,3], ListLEAF [4,5], ListNODE([ListLEAF [6], ListLEAF []]) ],
      ListNODE [],
      ListLEAF [7,8],
      ListNODE [ListLEAF [], ListLEAF []] ]

l1 = ListLEAF ["School","-","of","-","Electrical"]
l2 = ListLEAF ["-","Engineering","-"]
l3 = ListLEAF ["and","-","Computer","-"]
l4 = ListLEAF ["Science"]
l5 = ListLEAF ["-WSU"]
n1 = ListNODE [l1,l2]
n2 = ListNODE [n1,l3]
t5 = ListNODE [n2,l4,l5]

-- Provided sample tests
p5_test1 = TestCase (assertEqual ("foldListTree (+) 0 " ++ (show t4))
    36
    (foldListTree (+) 0 t4))

p5_test2 = TestCase (assertEqual ("foldListTree (++) \"\" " ++ (show t5))
    "School-of-Electrical-Engineering-and-Computer-Science-WSU"
    (foldListTree (++) "" t5))

-- Additional tests
p5_test3 = TestCase (assertEqual "foldListTree (+) 0 single ListLEAF"
    6
    (foldListTree (+) 0 (ListLEAF [1,2,3])))

p5_test4 = TestCase (assertEqual "foldListTree max on myListTree (max leaf = 12)"
    12
    (foldListTree max 0 myListTree))

p5_test5 = TestCase (assertEqual "foldListTree (+) 0 myListTree (1..12 = 78)"
    78
    (foldListTree (+) 0 myListTree))

p5_test6 = TestCase (assertEqual "foldListTree on empty ListNODE"
    0
    (foldListTree (+) 0 (ListNODE [])))

-- =============================================================================
-- TEST SUITE
-- =============================================================================

tests = TestList [
    -- Problem 1a: merge2
    TestLabel "Problem 1a - test1" p1a_test1,
    TestLabel "Problem 1a - test2 (empty first)" p1a_test2,
    TestLabel "Problem 1a - test3 (empty second)" p1a_test3,
    TestLabel "Problem 1a - test4 (all duplicates)" p1a_test4,
    TestLabel "Problem 1a - test5 (negatives)" p1a_test5,
    -- Problem 1b: merge2Tail
    TestLabel "Problem 1b - test1" p1b_test1,
    TestLabel "Problem 1b - test2 (both empty)" p1b_test2,
    TestLabel "Problem 1b - test3 (singletons)" p1b_test3,
    TestLabel "Problem 1b - test4 (matches merge2)" p1b_test4,
    -- Problem 1c: mergeN
    TestLabel "Problem 1c - test1" p1c_test1,
    TestLabel "Problem 1c - test2 (empty list)" p1c_test2,
    TestLabel "Problem 1c - test3 (single sublist)" p1c_test3,
    TestLabel "Problem 1c - test4 (empty sublist)" p1c_test4,
    -- Problem 2a: getInRange
    TestLabel "Problem 2a - test1" p2a_test1,
    TestLabel "Problem 2a - test2" p2a_test2,
    TestLabel "Problem 2a - test3 (boundary excluded)" p2a_test3,
    TestLabel "Problem 2a - test4 (empty input)" p2a_test4,
    TestLabel "Problem 2a - test5 (strings)" p2a_test5,
    -- Problem 2b: countInRange
    TestLabel "Problem 2b - test1" p2b_test1,
    TestLabel "Problem 2b - test2" p2b_test2,
    TestLabel "Problem 2b - test3 (empty nested)" p2b_test3,
    TestLabel "Problem 2b - test4 (all empty sublists)" p2b_test4,
    TestLabel "Problem 2b - test5 (none qualify)" p2b_test5,
    -- Problem 3a: addLengths
    TestLabel "Problem 3a - test1" p3a_test1,
    TestLabel "Problem 3a - test2" p3a_test2,
    TestLabel "Problem 3a - test3 (YARD+YARD)" p3a_test3,
    TestLabel "Problem 3a - test4 (FOOT+FOOT)" p3a_test4,
    TestLabel "Problem 3a - test5 (YARD+FOOT)" p3a_test5,
    -- Problem 3b: addAllLengths
    TestLabel "Problem 3b - test1" p3b_test1,
    TestLabel "Problem 3b - test2 (empty list)" p3b_test2,
    TestLabel "Problem 3b - test3 (empty sublists)" p3b_test3,
    TestLabel "Problem 3b - test4" p3b_test4,
    -- Problem 4a: sumTree
    TestLabel "Problem 4a - test1" p4a_test1,
    TestLabel "Problem 4a - test2 (single LEAF)" p4a_test2,
    TestLabel "Problem 4a - test3 (myTree)" p4a_test3,
    TestLabel "Problem 4a - test4 (symmetric)" p4a_test4,
    -- Problem 4b: createSumTree
    TestLabel "Problem 4b - test1" p4b_test1,
    TestLabel "Problem 4b - test2 (single LEAF)" p4b_test2,
    TestLabel "Problem 4b - test3 (myTree)" p4b_test3,
    TestLabel "Problem 4b - test4 (two-leaf)" p4b_test4,
    -- Problem 5: foldListTree
    TestLabel "Problem 5 - test1" p5_test1,
    TestLabel "Problem 5 - test2" p5_test2,
    TestLabel "Problem 5 - test3 (single leaf)" p5_test3,
    TestLabel "Problem 5 - test4 (max myListTree)" p5_test4,
    TestLabel "Problem 5 - test5 (sum myListTree)" p5_test5,
    TestLabel "Problem 5 - test6 (empty ListNODE)" p5_test6
    ]

-- Shortcut to run all tests
run = runTestTT tests

-- END TESTS
