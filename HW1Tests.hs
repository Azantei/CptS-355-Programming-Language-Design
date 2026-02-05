{-Haskell is available for Windows, Mac, and Linux. Here's the download page: http://www.haskell.org/platform/.

We will be using the HUnit unit testing package in CptS 355.
To setup HUnit:
1. First install call-stack
    - download the package from here: http://hackage.haskell.org/package/call-stack. Download the call-stack-0.2.0.tar.gz file and extract it.
    (To extract .gz and .tar files on Windows you may use 7zip (https://www.7-zip.org/))
    (To extract .gz and .tar files on Linux or Mac you may use gunzip and tar)
    - Then switch to the HUnit directory and install HUnit using the following commands at the terminal/command prompt:
            runhaskell Setup configure
            runhaskell Setup build
            runhaskell Setup install
    If you get "permission denied" errors,
        - on Windows  run the terminal or command line as administrator.
        - on Mac and Linux: run the command in 'sudo' mode (or login as root).

2. Next install HUnit:
   - download the package from here: http://hackage.haskell.org/package/HUnit. Download the HUnit-1.2.5.2.tar.gz file and extract it.
   - Then switch to the HUnit directory and install HUnit using the following commands at the terminal/command prompt:
            runhaskell Setup configure
            runhaskell Setup build
            runhaskell Setup install

            On a unix system or Mac, you may may need to be root for that last part.-}


{- Two useful functions in the HUnit package are assertEqual and assertBool.
The arguments to 'assertEqual' are:
      a descriptive string
      the expected value
      the value being tested
The arguments to 'assertBool' are:
      a descriptive string
      the boolean value being tested
-}

-- test1, test2, and test3 succeed
{- Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "runTestTT tests" at the Haskell prompt.  -}

module HW1Tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW1

prereqsList = [("Cpts122" , ["CptS121"]), ("CptS132" , ["CptS131"]), ("CptS223" , ["CptS122", "MATH216"]), ("CptS233" , ["CptS132", "MATH216"]),
                   ("CptS260" , ["CptS223", "CptS233"]), ("CptS315" , ["CptS223", "CptS233"]), ("CptS317" , ["CptS122", "CptS132", "MATH216"]),
                   ("CptS321" , ["CptS223", "CptS233"]), ("CptS322" , ["CptS223","CptS233"]), ("CptS350" , ["CptS223","CptS233", "CptS317"]),
                   ("CptS355" , ["CptS223"]), ("CptS360" , ["CptS223","CptS260"]),("CptS370" , ["CptS233","CptS260"]),
                   ("CptS427" , ["CptS223","CptS360", "CptS370", "MATH216", "EE234"])
                  ]

-- ========== PROBLEM 1 TESTS ==========
-- Given tests
p1_test1 = TestCase (assertEqual "(exists [1] [[3] [5]])" False  (exists [1] [[3],[5]]) )
p1_test2 = TestCase (assertBool "exists 1 [1,2,3]"  (exists 1 [1,2,3]) )
p1_test3 = TestCase (assertBool "exists '3' \"CptS355\"" (exists '3' "CptS355"))
p1_test4 = TestCase (assertEqual "exists '4' \"CptS355\"" False (exists '4' "CptS355"))

-- Additional testing
p1_test5 = TestCase (assertEqual "exists [2,3] [[1,2],[2,3],[4,5]]" True (exists [2,3] [[1,2],[2,3],[4,5]]))
p1_test6 = TestCase (assertEqual "exists 'x' \"\"" False (exists 'x' ""))
p1_test7 = TestCase (assertEqual "exists on nested empty list" False (exists [] [[[1]]]))
p1_test8 = TestCase (assertBool "exists with duplicate values" (exists 2 [1,2,2,3]))


-- ========== PROBLEM 2 TESTS ==========
-- Given tests
p2_test1 = TestCase (assertEqual "listUnion [1,3,4] [2,3,4,5]"  (sort [1,2,3,4,5])  (sort (listUnion [1,3,4] [2,3,4,5])) )
p2_test2 = TestCase (assertEqual "listUnion \"CptS355\" \"cpts322\""  (sort ("CptS35cs2"))  (sort (listUnion "CptS355" "cpts322" )) )
p2_test3 = TestCase (assertEqual "listUnion [[1,2],[2,3]] [[1],[2,3],[2,3]]"  (sort [[1,2],[1],[2,3]])  (sort (listUnion [[1,2],[2,3]] [[1],[2,3],[2,3]] ) ) )

-- Additional testing
p2_test4 = TestCase (assertEqual "listUnion [] []" ([] :: [Int]) (listUnion ([] :: [Int]) []))
p2_test5 = TestCase (assertEqual "listUnion [1,2,3] []" (sort [1,2,3]) (sort (listUnion [1,2,3] [])))
p2_test6 = TestCase (assertEqual "listUnion with all duplicates" (sort [1,2,3]) (sort (listUnion [1,1,2,2,3,3] [1,2,3])))
p2_test7 = TestCase (assertEqual "listUnion with nested lists" (sort [[1],[2],[3]]) (sort (listUnion [[1],[2],[2]] [[2],[3],[3]])))


-- ========== PROBLEM 3 TESTS ==========
-- Given tests
p3_test1 = TestCase (assertEqual "replace 3 40 [1, 2, 3, 4, 5, 6]" [1,2,3,40,5,6] (replace 3 40 [1, 2, 3, 4, 5, 6]) )
p3_test2 = TestCase (assertEqual "replace 0 'X' \"abcd\""  "Xbcd" (replace 0 'X' "abcd") )
p3_test3 = TestCase (assertEqual "replace 5 6 [1,2,3,4,5]" [1,2,3,4,5] (replace 5 6 [1,2,3,4,5]))

-- Additional testing
p3_test4 = TestCase (assertEqual "replace 10 'Z' \"abc\"" "abc" (replace 10 'Z' "abc"))
p3_test5 = TestCase (assertEqual "replace 2 100 [10,20,30,40]" [10,20,100,40] (replace 2 100 [10,20,30,40]))
p3_test6 = TestCase (assertEqual "replace at end of list" [1,2,3,99] (replace 3 99 [1,2,3,4]))
p3_test7 = TestCase (assertEqual "replace in single element list" ['X'] (replace 0 'X' "Y"))


-- ========== PROBLEM 4 TESTS ==========
-- Given tests
p4_test1 = TestCase (assertEqual "prereqFor prereqsList \"CptS260\"" (sort ["CptS360","CptS370"])  (sort (prereqFor prereqsList "CptS260")) )
p4_test2 = TestCase (assertEqual "prereqFor prereqsList \"CptS223\"" (sort ["CptS260","CptS315","CptS321","CptS322","CptS350","CptS355","CptS360",
            "CptS427"])  (sort (prereqFor prereqsList "CptS223")) )
p4_test3 = TestCase (assertEqual "prereqFor prereqsList \"CptS355\"" ([])  (sort (prereqFor prereqsList "CptS355")) )

-- Additional testing
p4_test4 = TestCase (assertEqual "prereqFor prereqsList \"MATH216\"" (sort ["CptS223","CptS233","CptS317","CptS427"]) (sort (prereqFor prereqsList "MATH216")))
p4_test5 = TestCase (assertEqual "prereqFor prereqsList \"CptS999\"" [] (sort (prereqFor prereqsList "CptS999")))
p4_test6 = TestCase (assertEqual "prereqFor with multiple dependencies" (sort ["CptS427"]) (sort (prereqFor prereqsList "CptS370")))
p4_test7 = TestCase (assertEqual "prereqFor on empty list" ([] :: [String]) (prereqFor [] "CptS223"))


-- ========== PROBLEM 5 TESTS ==========
-- Given tests
p5_test1 = TestCase (assertBool "isPalindrome \"a01 02 2010A\"" (isPalindrome "a01 02 2010A"))
p5_test2 = TestCase (assertBool "isPalindrome \"Doc note I dissent a fast never prevents a fatness I diet on cod\"" (isPalindrome "Doc note I dissent a fast never prevents a fatness I diet on cod"))
p5_test3 = TestCase (assertBool "isPalindrome \"Yreka Bakery\"" (isPalindrome "Yreka Bakery"))

-- Additional testing
p5_test4 = TestCase (assertEqual "isPalindrome \"hello\"" False (isPalindrome "hello"))
p5_test5 = TestCase (assertBool "isPalindrome \"Madam Im Adam\"" (isPalindrome "Madam Im Adam"))
p5_test6 = TestCase (assertEqual "isPalindrome single character" True (isPalindrome "A"))
p5_test7 = TestCase (assertEqual "isPalindrome with numbers and spaces" True (isPalindrome "12 3 21"))


-- ========== PROBLEM 6 TESTS ==========
-- Given tests
p6_test1 = TestCase (assertEqual "groupSumtoN 15 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]" [[1,2,3,4,5],[6,7],[8],[9],[10]] (groupSumtoN 15 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) )
p6_test2 = TestCase (assertEqual "groupSumtoN 11 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]" [[1,2,3,4],[5,6],[7],[8],[9],[10]] (groupSumtoN 11 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) )
p6_test3 = TestCase (assertEqual "groupSumtoN 55 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]" [[1,2,3,4,5,6,7,8,9,10]] (groupSumtoN 55 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) )

-- Additional testing
p6_test4 = TestCase (assertEqual "groupSumtoN 1 [1,2,3,4,5,6,7,8,9,10]" [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]] (groupSumtoN 1 [1,2,3,4,5,6,7,8,9,10]))
p6_test5 = TestCase (assertEqual "groupSumtoN 100 [1,2,3,4,5]" [[1,2,3,4,5]] (groupSumtoN 100 [1,2,3,4,5]))
p6_test6 = TestCase (assertEqual "groupSumtoN with all elements > N" [[5],[6],[7]] (groupSumtoN 3 [5,6,7]))
p6_test7 = TestCase (assertEqual "groupSumtoN with mixed large and small" [[1,2],[20],[3,4]] (groupSumtoN 10 [1,2,20,3,4]))


-- ========== TEST LIST ==========
tests = TestList [ TestLabel "Problem 1- test1 " p1_test1,
                   TestLabel "Problem 1- test2 " p1_test2,
                   TestLabel "Problem 1- test3 " p1_test3,
                   TestLabel "Problem 1- test4 " p1_test4,
                   TestLabel "Problem 1- test5 " p1_test5,
                   TestLabel "Problem 1- test6 " p1_test6,
                   TestLabel "Problem 1- test7 " p1_test7,
                   TestLabel "Problem 1- test8 " p1_test8,
                   TestLabel "Problem 2- test1 " p2_test1,
                   TestLabel "Problem 2- test2 " p2_test2,
                   TestLabel "Problem 2- test3 " p2_test3,
                   TestLabel "Problem 2- test4 " p2_test4,
                   TestLabel "Problem 2- test5 " p2_test5,
                   TestLabel "Problem 2- test6 " p2_test6,
                   TestLabel "Problem 2- test7 " p2_test7,
                   TestLabel "Problem 3- test1 " p3_test1,
                   TestLabel "Problem 3- test2 " p3_test2,
                   TestLabel "Problem 3- test3 " p3_test3,
                   TestLabel "Problem 3- test4 " p3_test4,
                   TestLabel "Problem 3- test5 " p3_test5,
                   TestLabel "Problem 3- test6 " p3_test6,
                   TestLabel "Problem 3- test7 " p3_test7,
                   TestLabel "Problem 4- test1 " p4_test1,
                   TestLabel "Problem 4- test2 " p4_test2,
                   TestLabel "Problem 4- test3 " p4_test3,
                   TestLabel "Problem 4- test4 " p4_test4,
                   TestLabel "Problem 4- test5 " p4_test5,
                   TestLabel "Problem 4- test6 " p4_test6,
                   TestLabel "Problem 4- test7 " p4_test7,
                   TestLabel "Problem 5- test1 " p5_test1,
                   TestLabel "Problem 5- test2 " p5_test2,
                   TestLabel "Problem 5- test3 " p5_test3,
                   TestLabel "Problem 5- test4 " p5_test4,
                   TestLabel "Problem 5- test5 " p5_test5,
                   TestLabel "Problem 5- test6 " p5_test6,
                   TestLabel "Problem 5- test7 " p5_test7,
                   TestLabel "Problem 6- test1 " p6_test1,
                   TestLabel "Problem 6- test2 " p6_test2,
                   TestLabel "Problem 6- test3 " p6_test3,
                   TestLabel "Problem 6- test4 " p6_test4,
                   TestLabel "Problem 6- test5 " p6_test5,
                   TestLabel "Problem 6- test6 " p6_test6,
                   TestLabel "Problem 6- test7 " p6_test7
                 ]


-- shortcut to run the tests
run = runTestTT tests
-- End of HW1Tests.hs