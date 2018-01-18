{-# OPTIONS_GHC -Wall #-}

module Polikarpova06 where


newtype Poly a = P [a]


-- Задача 1 -----------------------------------------


x :: Num a => Poly a

x = P [0,1]

-- Задача 2 ----------------------------------------

check :: (Num a, Eq a) => [a] -> [a] -> Bool
check [] [] = True
check xs [] | null (filter (/=0) xs) = True
 | otherwise = False
check [] ys | null (filter (/=0) ys) = True
 | otherwise = False
check xs ys | head xs == head ys = check (tail xs) (tail ys)
 | otherwise = False

instance (Num a, Eq a) => Eq (Poly a) where
 
 (P xs) == (P ys) = check xs ys

-- Задача 3 -----------------------------------------


instance (Num a, Eq a, Show a) => Show (Poly a) where

 show (P [0]) = "0" 
 show (P [b] )= show b
 show (P []) = "0"
 show (P [0,1]) = "x"
 show (P [0,-1]) = "-x"
 show (P xs) 
  | (length xs == 2)&&((head xs) == 0)&&(last xs /=0) = (show (last xs)) ++ "x"
  | null (filter (/=0) (init xs))&&(last xs /= 0)&&(last xs /=1)&& (last xs /= -1) = show (last xs) ++ "x^" ++ (show (length xs - 1)) 
  | null (filter (/=0) (init xs))&&(last xs /= 0)&&(last xs==1) = "x^" ++ (show (length xs - 1)) 
  | null (filter (/=0) (init xs))&&(last xs /= 0)&&(last xs== -1) = "-x^" ++ (show (length xs - 1)) 
  | (last xs) == 0 = show (P (init xs)) 
  | (length xs == 2)&&((last xs) == 1) = "x" ++ " + " ++ show (P (init  xs)) 
  | (length xs == 2)&&((last xs) == -1) = "-x" ++ " + " ++ show (P (init  xs)) 
  | length xs == 2 = (show (last xs)) ++ "x" ++ " + "++ show (P (init    xs)) 
  | (last xs) == 1 = "x^" ++ (show (length xs - 1))++ " + " ++   show (P (init xs))   
  | (last xs) == -1 = "-x^" ++ (show (length xs - 1))++ " + " ++   show (P (init xs)) 
  | otherwise =  (show (last xs)) ++ "x^" ++ (show (length xs - 1)) ++ " + "++ show (P (init xs)) 


-- Задача 4 -----------------------------------------


sumL :: Num a => [a] -> [a] -> [a]
sumL [] [] = []
sumL xs [] = xs
sumL [] ys = ys
sumL (a:xs) (b:ys) = (a + b):(sumL xs ys)

plus :: Num a => Poly a -> Poly a -> Poly a

plus (P xs) (P ys) = P (sumL xs ys)

-- Задача 5 -----------------------------------------


form :: Num a => a -> [a] -> [a]
form _ [] = []
form a (b:ys) = a*b: form a ys

mult :: Num a => [a] -> [a] -> [a]
mult [] [] = []
mult _ [] = []
mult [] _ = []
mult (a:xs) ys = sumL (form a ys) (0:mult xs ys)

times :: Num a => Poly a -> Poly a -> Poly a

times (P xs) (P ys) = P (mult xs ys)

-- Задача 6 -----------------------------------------


instance Num a => Num (Poly a) where
    
  (P xs) + (P ys) = plus (P xs) (P ys)   
  (P xs) * (P ys)  = times (P xs) (P ys)      
  negate (P ys) = P (map negate ys)
  fromInteger y = P [fromInteger y]
-- Розумних означень не існує
    
  abs    = undefined
    
  signum = undefined


-- Задача 7 -----------------------------------------

 
applyP :: Num a => Poly a -> a -> a

applyP (P []) _ = 0
applyP (P ps) t = if (length ps /= 1) then
 (last ps) * ( t ^ (length ps - 1)) Prelude.+ applyP (P (init ps)) t
 else last ps



-- Задача 8 -----------------------------------------


class Num a => Differentiable a where
    
  deriv:: a -> a
    
  nderiv :: Int -> a -> a
    
  nderiv 1 a = deriv a
  nderiv n a = nderiv (n-1) (deriv a)


-- Задача 9 -----------------------------------------
index:: Num a => [a] -> a
index [] = 0
index (_:as) = 1 + index as

d :: Num a => [a]  -> [a]
d [] = []
d ps = reverse (if (length ps /= 1) then ((last ps) * index (tail ps)): d (init ps) else [])


instance Num a => Differentiable (Poly a) where
   --deriv (P ps) = (P ps)  
  deriv (P ps) = P (d ps)

