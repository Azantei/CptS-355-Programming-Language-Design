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
--merge2


--merge2Tail


--mergeN


{- 2 - getInRange & countInRange - 18% -}

--getInRange


--countInRange



{- 3 -  addLengths & addAllLengths - 18% -}

data LengthUnit =  INCH  Int | FOOT  Int | YARD  Int
                   deriving (Show, Read, Eq)
-- addLengths

-- addAllLengths

{-4 - sumTree and createSumTree - 22%-}

data Tree a = LEAF a | NODE a  (Tree a)  (Tree a)
              deriving (Show, Read, Eq)

--sumTree


--createSumTree


{-5 - foldListTree - 20%-}
data ListTree a = ListLEAF [a] | ListNODE [(ListTree a)]
                  deriving (Show, Read, Eq)



{- 6- Create two tree values :  Tree Integer  and  listTree a ;  Both trees should have at least 3 levels. -}