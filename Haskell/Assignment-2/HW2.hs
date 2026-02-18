-- CptS 355 - Spring 2026 Assignment 2
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework
-- Also include a statement identifying the extent of your AI use (to include "None" if appropriate) and a log of your AI use (or attacched in .txt, .md, or .pdf format)
  
{- 
Name: Camille Orego
GitHub Repo: https://github.com/Azantei/CptS-355-Programming-Language-Design
Helpers: Claude Sonnet 4.5 
        (in log conversations on GitHub: https://github.com/Azantei/CptS-355-Programming-Language-Design/tree/main/Haskell/Assignment-2/Logs)
-}   

module HW2
     where


{- 1-  merge2 & merge2Tail & mergeN - 22% -}

-- merge2
-- Takes two sorted lists and merges them into one sorted list.
merge2 :: Ord a => [a] -> [a] -> [a]
-- Base cases: if one list is empty, return the other list
merge2 [] l2 = l2
merge2 l1 [] = l1
-- Otherwise, compare the heads of both lists and merge accordingly
merge2 (x:xs) (y:ys)
     -- If x is less than or equal to y, x goes first, followed by the merge of the rest of l1 and all of l2
     | x <= y    = x : merge2 xs (y:ys) -- x is the head
     -- If y is less than x, y goes first, followed by the merge of all of l1 and the rest of l2
     | otherwise = y : merge2 (x:xs) ys -- y is the head

-----------------------------------------------------------------

-- merge2Tail
-- Uses tail recursion to take two sorted lists and merges them into one sorted list.
merge2Tail :: Ord a => [a] -> [a] -> [a]

merge2Tail l1 l2 = merge2TailHelper l1 l2 []
    where
        -- Base cases: empty list calls revAppend on existing list
        merge2TailHelper [] l2 acc = revAppend acc l2
        merge2TailHelper l1 [] acc = revAppend acc l1
        -- Otherwise, compare the heads of both lists and merge accordingly
        merge2TailHelper (x:xs) (y:ys) acc
               -- If x is less than or equal to y, we push x onto head of accumulator list
               | x <= y    = merge2TailHelper xs (y:ys) (x:acc)
               -- If y is less than x, we push y onto head of accumulator list
               | otherwise = merge2TailHelper (x:xs) ys (y:acc)

-- revAppend function for merge2Tail (above)
revAppend :: [a] -> [a] -> [a]
-- Base case: if the first list is empty, return the second list 
--(don't need to reverse anything - especially if accumulator list from merge2TailHelper is empty)
revAppend [] ys = ys
-- Append the head of the first list to the head of the second list
-- (reverse the first list since that is the one being built up in reverse order)
revAppend (x:xs) ys = revAppend xs (x:ys)

-----------------------------------------------------------------

-- mergeN
-- Takes a list of sorted lists and merges them all into one sorted list. 
mergeN :: Ord a => [[a]] -> [a]
-- use foldl to repeatedly apply merge2 to the list of lists,
-- starting with an empty list as the initial accumulator value
-- foldl parameters: foldl function startingValue list
mergeN lists = foldl merge2 [] lists

-----------------------------------------------------------------
-----------------------------------------------------------------

{- 2 - getInRange & countInRange - 18% -}

-- getInRange
-- Takes two values: v1 and v2, and a list. Returns the elements that are strictly between those values.
getInRange :: Ord a => a -> a -> [a] -> [a]
-- filter: (predicate function) (list to filter)
-- filter :: (a -> Bool) -> [a] -> [a]
-- Uses filter and a lamda function (expressing filtered range) + iL (list) to return a list of elements that are strictly between v1 and v2
getInRange v1 v2 iL = filter (\x -> x > v1 && x < v2) iL

-----------------------------------------------------------------

-- countInRange
-- Takes two values: v1 and v2, and a nested list. Returns the total count of elements strictly between those values across all sublists.
countInRange :: Ord a => a -> a -> [[a]] -> Int
-- Use function composition: start with innermost function to map the in-range values. 
-- Map the length of that result so we can use foldl to sum the count of in-range values.
countInRange v1 v2 iL = foldl (+) 0 (map length (map (getInRange v1 v2) iL))
-- foldl (+) 0 explicit version of "sum" function

-----------------------------------------------------------------
-----------------------------------------------------------------

{- 3 -  addLengths & addAllLengths - 18% -}

data LengthUnit =  INCH  Int | FOOT  Int | YARD  Int
                   deriving (Show, Read, Eq)
-- addLengths
-- Adds two LengthUnit values together and returns the result as an INCH constructor.

-- Function to convert each constructor to inches
toInches :: LengthUnit -> Int
toInches (INCH n) = n
toInches (FOOT n) = n * 12
toInches (YARD n) = n * 36

addLengths :: LengthUnit -> LengthUnit -> LengthUnit
-- Use conversion to add the two lengths together.
-- Return the result as an INCH constructor
addLengths l1 l2 = INCH (toInches l1 + toInches l2)

-----------------------------------------------------------------

-- addAllLengths
-- Takes a nested list of LengthUnit values and returns the total length as an INCH constructor.

addAllLengths :: [[LengthUnit]] -> LengthUnit
-- Inner parenthesis: foldl repeatedly adds the lengths of sublists together.
-- Map puts results of sublists into a list.
-- foldl is called again to add those results for a single value.
addAllLengths iL = foldl addLengths (INCH 0) (map (foldl addLengths (INCH 0)) iL)

-----------------------------------------------------------------
-----------------------------------------------------------------

{-4 - sumTree and createSumTree - 22%-}

data Tree a = LEAF a | NODE a  (Tree a)  (Tree a)
              deriving (Show, Read, Eq)

-- sumTree
-- Takes a Tree of Nums and returns the sum of all the leaf values in the tree.
sumTree :: Num p => Tree p -> p
sumTree (LEAF a) = a
sumTree (NODE a left right) = sumTree left + sumTree right

-----------------------------------------------------------------

--createSumTree
-- Rebuilds the tree with each node storing the sum of its leaf values below it.
createSumTree :: Num a => Tree a -> Tree a
-- Base case: Leaf
createSumTree (LEAF a) = LEAF a 
-- Building subtrees:
-- Node a (Tree a) (Tree b)
-- Rebuilds the tree, using sumTree to calculate and store the sum of leaf values in each node.
createSumTree (NODE a left right) = NODE (sumTree left + sumTree right) (createSumTree left) (createSumTree right)


{-5 - foldListTree - 20%-}
data ListTree a = ListLEAF [a] | ListNODE [(ListTree a)]
                  deriving (Show, Read, Eq)


-----------------------------------------------------------------
-----------------------------------------------------------------

{- 6- Create two tree values :  Tree Integer  and  listTree a ;  Both trees should have at least 3 levels. -}