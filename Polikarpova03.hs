{-# OPTIONS_GHC -Wall #-}
module Polikarpova03 where

data OrdTree a = OrdTree a [OrdTree a]  
               deriving (Show)

data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving (Show, Eq)

-- Задача 1 -----------------------------------------
getRoots :: OrdTree a -> [a]
getRoots (OrdTree v []) = [v]
getRoots (OrdTree v t ) = v: concat (map getRoots t)

dfsTree ::  [OrdTree a] -> [a]
dfsTree ts = concat (map getRoots ts)
  
-- Задача 2 ----------------------------------------- 
getRoot :: OrdTree a -> a
getRoot (OrdTree v _ ) = v

getBranches :: OrdTree a -> [OrdTree a]
getBranches (OrdTree _ []) = []
getBranches (OrdTree _ t) = t

bfsTree ::  [OrdTree a] -> [a]
bfsTree [] = []
bfsTree ts = map getRoot ts ++ bfsTree (concat (map getBranches ts))

-- Задача 3 -----------------------------------------
-- Дерева рівні, якщо мають однакові значення та однакову структуру ([1,5,7] != [1,7,5])
check ::(Eq a)=> [a] -> [a] -> Bool
check [] [] = True
check _ [] = False
check [] _ = False
check t1 t2 = if (head t1)==(head t1) then check (tail t1) (tail t2)
 else False

equalTree ::(Eq a)=> OrdTree a -> OrdTree a -> Bool
equalTree tr1 tr2 = check (getRoots tr1) (getRoots tr2)

-- Задача 4 -----------------------------------------
toBinTree :: [OrdTree a] -> BinTree a
toBinTree [] = Empty
toBinTree (x:xs) = Node (getRoot x) (toBinTree (getBranches x)) (toBinTree xs)

-- Задача 5 -----------------------------------------
toOrdTree :: BinTree a -> [OrdTree a]  
toOrdTree Empty = []
toOrdTree (Node v tl tr) = OrdTree v (toOrdTree tl):(toOrdTree tr)

-- Задача 6 -----------------------------------------
is :: (a -> Bool) -> BinTree a -> Bool
is _ Empty = True
is p (Node v1 l r) = if p v1 then (is p l)&&(is p r)
 else False

isSearch :: (Ord a) => BinTree a -> Bool
isSearch Empty = True
isSearch (Node _ Empty Empty) = True
isSearch (Node v tl tr) = (is (>v) tr)&&(is (<v) tl)&&(isSearch tl)&&(isSearch tr)

-- Задача 7  -----------------------------------------
elemSearch ::(Ord a) => BinTree a -> a -> Bool
elemSearch Empty _ = False
elemSearch (Node v tl tr) e = if (v/=e) then
 if (v>e) then elemSearch tl e else elemSearch tr e
 else True

-- Задача 8 ------------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a
insSearch Empty v = Node v Empty Empty

insSearch (Node v Empty Empty) e = if (e>v) then (Node v Empty (Node e Empty Empty))
 else  if (e<v) then (Node v(Node e Empty Empty)  Empty) else (Node v Empty Empty) 

insSearch (Node v tl tr) e = if (e>v) then (Node v tl (insSearch tr e))
 else if (e<v) then (Node v (insSearch tl e) tr) else (Node v tl tr) 

-- Задача 9 ------------------------------------------
minB:: (Ord a) => BinTree a -> a
minB (Empty) = error "Empty tree"
minB (Node v Empty _ ) = v
minB (Node _ tl _) = minB tl

delSearch :: (Ord a) => BinTree a -> a -> BinTree a
delSearch  Empty _ = Empty

delSearch (Node v Empty Empty) e = if (e==v) then Empty 
 else (Node v Empty Empty)

delSearch (Node v tl Empty) e = if (e==v) then tl
 else if (e<v) then (Node v (delSearch tl e) Empty) else (Node v tl Empty)

delSearch (Node v Empty tr) e = if (e==v) then tr
 else if (e>v) then (Node v Empty (delSearch tr e)) else (Node v Empty tr)

delSearch (Node v tl tr) e = if (e>v) then (Node v tl (delSearch tr e))
 else if (e<v) then (Node v (delSearch tl e) tr) else let m = minB tr
 in (Node m tl (delSearch tr m))

-- Задача 10 -----------------------------------------
toArray :: (Ord a) => BinTree a -> [a]
toArray (Node v Empty Empty) = [v]
toArray t = minB t: toArray (delSearch t (minB t))

sortList :: (Ord a) => [a] -> [a]
sortList [] = []
sortList xs = toArray (foldl insSearch Empty xs)

---------------------------------
otx :: [OrdTree Int]
otx =  [ OrdTree 1 [OrdTree 2 [], 
                    OrdTree 3 [OrdTree 10 []] ] ,
         OrdTree 4 [OrdTree 5 [OrdTree 8 []], 
                               OrdTree 6 [OrdTree 9 []],
                               OrdTree 7 []]  
       ] 

otx2 :: OrdTree Int
otx2 =  OrdTree 1 []



bt :: BinTree Int
bt = Node 1(Node 2 Empty
                    (Node 3 (Node 10 Empty Empty)
                            Empty)
            ) 
            (Node 4  (Node 5 (Node 8  Empty Empty)
                             (Node 6  (Node 9 Empty Empty)
                                      (Node 7 Empty Empty)
                             )
                      )
             Empty
            )

bt2 :: BinTree Int
bt2 = Node 40 (Node 20 (Node 1  Empty Empty) (Node 31 (Node 30 Empty Empty) (Node 33 Empty Empty))) (Node 60  Empty Empty)

bt3 :: BinTree Int
bt3 = Node 4 Empty (Node 5 (Node 2  Empty Empty) Empty)
            

