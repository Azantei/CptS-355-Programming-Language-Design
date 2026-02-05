-- CptS 355 - Spring 2026 Assignment 1
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework
-- Name: Camille
-- Helpers: Claude Sonnet 4.5 (in log conversations on GitHub: https://github.com/Azantei/CptS-355-Programming-Language-Design)

module HW1
     where

import Data.Char (toUpper)

-- 1. exists
{-
Type class Eq. Takes a “value” and a “list” as input. Returns boolean. 
If the value is a member of the list, the function returns True. 
Otherwise returns False.
-}
exists :: Eq t => t -> [t] -> Bool
exists value [] = False
exists value (x:xs)
     | value == x = True
     | otherwise = exists value xs
{-
b) We need the Eq type class because we are comparing values to members of the same type.
This tells the compiler we will be using equality operations on the type t.
-}


-- 2. listUnion
{-
Takes two lists as input and returns the union of those lists minus duplicate values.
Order does not matter.
-}
listUnion :: Eq a => [a] -> [a] -> [a]
listUnion xs ys = 
    let removeDups [] = []
        removeDups (z:zs) = if exists z zs
                            then removeDups zs
                            else z : removeDups zs
    in
       case (xs, ys) of
           ([], []) -> []
           ([], _) -> removeDups ys
           (_, []) -> removeDups xs     
           ((x:xs'), (y:ys')) -> if exists x xs' || exists x (y:ys')
                                 then listUnion xs' (y:ys')
                                 else x : listUnion xs' (y:ys')


-- 3. replace
{-
Takes an index n, a value v, and a list L and returns a (new) list which is the same as L, 
  except that its nth element is v. 
Assume 0-based indexing for n and n≥0. (Note that n can be greater than the length of the 
  list L. ) 
-}
replace :: (Eq t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
replace n v [] = []
replace 0 v (x:xs) = v : xs
replace n val (x:xs) = x : replace (n-1) val xs


-- 4. prereqFor
{-
Takes a list of courses (tuples of course name and prerequisite list) and a target course.
Returns a list of course names that require the target course as a prerequisite.
-}
prereqFor :: Eq t => [(a, [t])] -> t -> [a]
prereqFor [] _ = []
prereqFor ((courseName, prereqList) : tail) targetCourse
    | exists targetCourse prereqList = courseName : prereqFor tail targetCourse
    | otherwise = prereqFor tail targetCourse


-- 5. isPalindrome
{-
Takes a string and returns True if it's a palindrome (ignoring spaces and case),
False otherwise.
-}
isPalindrome :: [Char] -> Bool
isPalindrome str = 
    let removeSpaces [] = []
        removeSpaces (x:xs) 
            | x == ' ' = removeSpaces xs
            | otherwise = x : removeSpaces xs
        
        toUppercase [] = []
        toUppercase (x:xs) = toUpper x : toUppercase xs
        
        cleanStr = toUppercase (removeSpaces str)
    in cleanStr == reverse cleanStr


-- 6. groupSumtoN
{-
Takes an integer N and a list of integers L.
Groups consecutive elements into sublists where each sublist's sum is <= N.
If an element is greater than N, it gets its own sublist.
-}
groupSumtoN :: (Ord a, Num a) => a -> [a] -> [[a]]
groupSumtoN n [] = [[]]
groupSumtoN n list = 
    let helper [] [] _ = []
        helper [] currentGroup _ = [currentGroup]
        
        helper (x:xs) currentGroup currentSum
            | x > n && null currentGroup = [x] : helper xs [] 0
            | x > n = currentGroup : [x] : helper xs [] 0
            | currentSum + x <= n = helper xs (currentGroup ++ [x]) (currentSum + x)
            -- FIX: Start fresh with x if currentGroup is empty
            -- Prevent empty list creation
            | null currentGroup = helper (x:xs) [x] x
            | otherwise = currentGroup : helper (x:xs) [] 0
    
    in helper list [] 0
-- End of HW1.hs
