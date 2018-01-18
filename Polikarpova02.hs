{-# OPTIONS_GHC -Wall #-}
module Polikarpova02 where

-- Задача 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl ix = foldl (+) (head ix) (tail ix)
  
-- Задача 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr ix = foldr (*) 1 ix

-- Задача 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- Задача 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert [] v = [v]
insert ix v = if (v<=(head ix)) then v:head ix:tail ix
 else head ix: insert (tail ix) v

sortInsert :: [Int] -> [Int]
sortInsert [] = []
sortInsert ix  = foldl (insert) [head ix] (tail ix)

-- Задача 5 -----------------------------------------
index :: (Int -> Bool) -> Int -> [Int] -> [Int] 
index _ _ [] = []
index p s lx = if p (head lx) then 
 s:index p (s+1) (tail lx)
 else index p (s+1) (tail lx)

findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices _ [] = []
findIndices p lx = index p (0) lx

-- Задача 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse [] = []
allReverse sx = reverse (map reverse sx)

-- Задача 7  -----------------------------------------
letters :: Char -> Bool
letters lx | (elem '0' [lx]|| elem '1' [lx]||elem '2' [lx]||elem '3' [lx]||elem '4' [lx]||elem '5' [lx]||elem '6' [lx]||elem '7' [lx]|| elem '8' [lx]||elem '9' [lx]) = False
 | otherwise = True

noDigits :: String -> String
noDigits lx = if null lx then ""
 else filter (letters) lx

-- Задача 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood [] _ = 0
cntGood px v = if (head px) v then 1 + cntGood (tail px) v
 else cntGood (tail px) v
-- cntGood px v = length(filter ($v) px)
-- cntGood px v = length(filter (\p -> p v) px)

-- Задача 9 ------------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- Задача 10 -----------------------------------------
piramid :: [Integer]
piramid  = scanl1 (+) [x*x|x<-[1..]]

-- Задача 11 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1:zipWith (*) factorialsM [2..]

