{-# OPTIONS_GHC -Wall #-}
module Polikarpova10 where
import Data.Char
import Control.Monad
import Data.List


-- Çàäà÷à 1 ------------------------------------------
ints:: Int -> Char -> [Char]
ints n c = if (n==1) then [] else c:ints (n-1) c

listN:: Int -> [Int]
listN n = [a..z]
          where 
          a = read ("1"++ints n '0');
          z = read ("9"++ints n '9')

sumA:: String -> Int -> Int
sumA [] _= 0
sumA (s:xs) n = if (n==0) then 0 else (digitToInt s + (sumA xs (n-1)))

sumB:: String -> Int -> Int
sumB [] _= 0
sumB xs n = if (n==0) then 0 else digitToInt (last xs) + (sumB (init xs) (n-1))

check:: [String] -> Int -> [String]
check [] _ = []
check (x:xs) n = if ((sumA x n) == (sumB x n) ) then x:check xs n else check xs n

lucky ::   Int ->  [String]
lucky 0 = []
lucky n =  check (map show (listN (2*n))) n

-- Çàäà÷à 2 ----------------------------------------- 
queens ::  Int -> [[Int]]
queens n = map fst $ foldM findQs ([],[1..n]) [1..n]

findQs :: ([Int], [Int]) -> p -> [([Int], [Int])]
findQs (y,d) _ = [(x:y, delete x d) | x <- d, (good x y)]

good :: Int -> [Int] -> Bool
good x y = and [x /= c + n && x /= c - n | (n,c) <- zip [1..] y]

-- Çàäà÷à 3 -----------------------------------------
ml:: [[Int]] -> Int -> Int
ml [] mx = mx
ml (x:xs) mx = if ((length x) > mx) then ml xs (length x) else ml xs mx

maxLen ::  [Int] -> Int
maxLen xs = ml (allL xs) 0
   
-- Çàäà÷à 4 -----------------------------------------
maxSeq ::  [Int] ->  [Int]
maxSeq xs = head (allMaxSeq xs)

-- Çàäà÷à 5 -----------------------------------------
notSec:: [Int] -> Bool
notSec [] = True
notSec (_:[]) = True
notSec xs = if ((head xs) /= (head $ tail xs)) && (notSec (tail xs)) then True else False

findAll:: [Int] -> Bool
findAll xs = if ((sort xs) == xs) && (notSec xs) then True else False

allL:: [Int] -> [[Int]]
allL xs = filter findAll (subsequences xs)

findLists:: Int -> [[Int]] -> [[Int]]
findLists _ [] = []
findLists l (x:xs) = if (length x == l) then x:findLists l xs else findLists l xs

allMaxSeq ::  [Int] -> [[Int]]
allMaxSeq xs = findLists (maxLen xs) (allL xs)

-- Çàäà÷à 6 -----------------------------------------
halve:: [Int] -> ([Int],[Int])
halve [x] = ([x],[])
halve xs = ((hv1 xs 0), (reverse (hv2 xs 0)))

hv1::[Int]->Int -> [Int]
hv1 [] _ = []
hv1 xs c = if (c<=((length xs) `div` 2)) then (head xs:hv1 (tail xs) (c+1)) else []

hv2::[Int]->Int -> [Int]
hv2 [] _ = []
hv2 xs c = if (c<=(((length xs) `div` 2) + 1)) then (last xs:hv2 (init xs) (c+1)) else []







genExpr:: Int ->Int -> [String]
genExpr n b  = del (map show (filter ((== Just b) . eval) (expses (reverse (nums n)))))

del :: [String] -> [String]
del = delH []
    where delH old [] = old
          delH old (x:xs)
              | x `elem` old = delH old xs
              | otherwise = delH (old ++ [x]) xs

data Op = Op {
    op :: Int -> Int -> Int,
    name :: String,
    isEquals :: Int -> Int -> Bool }

data Ex = Value Int | Ex Op Ex Ex 
 
nums :: Int -> [Int]
nums n = if (n < 0) then [] else if n < 10 then [n] else n `mod` 10 : nums (n `div` 10)

expses :: [Int] -> [Ex]
expses [x] = [Value x]
expses xs = do
        o <- [ Op (+) "+" (const (const True)), Op (-) "-" (const (const True)), Op (*) "*" (const (const True))]
        pos <- [1..n-1]
        l <- expses (take pos xs)
        r <- expses (drop pos xs)
        return (Ex o l r)
        where 
          n = length xs

eval :: Ex -> Maybe Int
eval (Value v) = Just v
eval (Ex o l r) = do
    le <- eval l
    re <- eval r
    if isEquals o le re then Just (op o le re) else Nothing
 
instance Show Ex where
    show (Value v) = show v
    show (Ex o l r) = show l ++ name o ++ show r

-- Çàäà÷à 7 -----------------------------------------

genExprBracket ::  Int -> Int -> [String]
genExprBracket  n b = map show (filter ((== Just b) . evalP) (expsesP (reverse (nums n))))

data ExP = ValueP Int | ExP Op ExP ExP

expsesP :: [Int] -> [ExP]
expsesP [x] = [ValueP x]
expsesP xs = do
        o <- [ Op (+)  "+" (const (const True)), Op (-) "-" (const (const True)), Op (*) "-" (const (const True))]
        pos <- [1..n-1]
        l <- expsesP (take pos xs)
        r <- expsesP (drop pos xs)
        return (ExP o l r)
        where 
          n = length xs
 
evalP :: ExP -> Maybe Int
evalP (ValueP v) = Just v
evalP (ExP o l r) = do
    le <- evalP l
    re <- evalP r
    if isEquals o le re then Just (op o le re) else Nothing
 
instance Show ExP where
    show (ValueP v) = show v
    show (ExP o l r) = "(" ++ show l ++ name o ++ show r ++ ")" 

