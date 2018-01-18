{-# OPTIONS_GHC -Wall #-}
module Polikarpova01 where


-- Задача 1 -----------------------------------------

factorial :: Integer -> Integer

factorial n = if (n<0) then 0 else 
 if n==0 then 1 
 else n*factorial (n-1)
-- Задача 2 -----------------------------------------

listSum :: [Int]-> [Int] -> [Int]

listSum xs ys = if null xs then ys
 else if null ys then xs 
 else (head xs + head ys): listSum (tail xs) (tail ys)
-- Задача 3 -----------------------------------------

oddEven :: [Int] -> [Int]

oddEven [] = []
oddEven [x] = [x]
oddEven lx = head(tail lx):head lx:oddEven (tail(tail lx))
-- Задача 4 -----------------------------------------

power3 :: [Integer]
power3 = [x*x*x|x <- [1..]]
-- Задача 5 -----------------------------------------

toPower3 :: [Integer]

toPower3 = [v|x <- [3], y <- [1..], let v = x^y]
-- Задача 6 -----------------------------------------

sumPower3 :: Integer -> Integer

sumPower3 n = sum [v|x <- [3], y <- [1..n], let v = x^y]
-- Задача 7 -----------------------------------------

sumPower :: Integer -> Integer -> Integer
sumPower m n = if (m<0) then 0 else sum [v| i <- [1..n], let v = m^i]
-- Задача 8 -----------------------------------------

expPart :: Integer -> Integer -> Double

expPart m n = if (m<0) then 0 
 else sum [v| i <- [1..n], let v = fromIntegral (m^i)/ (fromIntegral (factorial i))]
-- Задача 9 -----------------------------------------

factorialsM :: [Integer]

factorialsM = [v|x <- [1..], let v = factorial x] 

-- Задача 10 -----------------------------------------
set:: [Int] -> [Int]
set xs = if null xs then xs else
 (head xs): set (filter (/=head xs) (tail xs))

cnt:: [Int] -> [Int]
cnt xs = (length (filter (==head xs) xs)): cnt (filter (/=head xs) (tail xs))

frequency :: [Int] -> [(Int,Int)]

frequency lx = zip (set lx) (cnt lx)