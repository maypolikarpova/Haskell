{-# OPTIONS_GHC -Wall #-}
module Polikarpova07 where

data Stream a = Cons a (Stream a)

--Show ???? ??? 20 ????, ? ??? ?????? ??? ??????
instance Show a => Show (Stream a) where
    show xs =  (foldl (++) "[" 
                  $ map (\x -> (show x) ++ ", ") $ take 20 $ streamToList xs
                ) ++ "..."
				
-- ????1 -----------------------------------------

streamToList :: Stream a -> [a]
streamToList (Cons a st) = a:streamToList st

-- ????2 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a st) = Cons (f a) (fmap f st)

-- ????3 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat v = Cons v (sRepeat v)

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a (sIterate f (f a))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a st) (Cons b st2) = Cons a (Cons b (sInterleave st st2))

sTake :: Int -> Stream a -> [a]
sTake s a = take s (streamToList a)

-- ????4 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

-- ????5 -----------------------------------------
fTwo :: Integer -> Integer -> Integer
fTwo n a = if (n `mod` (2^a))==0 then fTwo n (a+1) else a-1

makeStream::(Integer->Integer->Integer)->[Integer]->Stream Integer
makeStream f (x:xs) = Cons (f x 1) (makeStream f xs)

ruler :: Stream Integer
ruler = makeStream fTwo [1..]

-- ????6 -----------------------------------------
random::Integer -> Integer
random r = (1103515245 * r + 12345) `mod` 2147483648

rand :: Integer -> Stream Integer
rand r = Cons r (rand (random r))

-- ????7 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- ????8 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)

-- ????9 -----------------------------------------

data  Matrix a = M(a,a)(a,a)
         deriving (Show, Eq, Ord)
         
instance Num a => Num (Matrix a) where
 
 M(a,a1)(a2,a3) + M(b,b1)(b2,b3) = M(a+b,a1+b1) (a2+b2,a3+b3)
 M(a1,b1)(a2,b2) * M(c1,d1)(c2,d2) = M(a1*c1+b1*c2,a1*d1+b1*d2)(a2*c1+b2*c2,a2*d1+b2*d2)
 negate  (M(a,a1)(a2,a3))   =  M(negate a,negate a1) (negate a2,negate a3)
 fromInteger a = M (fromInteger a,0) (0,0)

 abs    = undefined
 signum = undefined

-- ????10 ----------------------------------------
takeI:: (Matrix Integer)->Integer
takeI (M(_,_)(a2,_)) = a2


fastFib:: Integer -> Integer
fastFib n = takeI (M(1,1)(1,0) ^ n)