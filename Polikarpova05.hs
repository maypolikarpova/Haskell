{-# OPTIONS_GHC -Wall #-}
module Polikarpova05 where

-- Задача 1 -----------------------------------------
getNameFile :: String -> IO String
getNameFile prompt = do 
 putStr prompt
 getLine

printN:: [String] -> String -> Int -> String
printN [] _ _ = ""
printN _ "0" _ = ""
printN s n c = if (show(c)==n) then printN [] n c
 else (head s) ++ "\n" ++ (printN (tail s) n (c+1))

firstN :: IO()
firstN = do
 f <- getNameFile "File> "
 s <- readFile f
 putStr "N> "
 n <- getLine
 putStr (printN (lines s) n 0)
  
-- Задача 2 ----------------------------------------- 
findTail:: [String] -> String -> Int -> [String]
findTail [] _ _ = []
findTail _ "0" _ = []
findTail s n c = if (show(c)==n) then findTail [] n c
 else (last s):(findTail (init s) n (c+1))

printTail:: [String] -> String 
printTail [] = ""
printTail xs = (last xs) ++ "\n" ++ (printTail (init xs))

tailN :: IO()
tailN = do
 f <- getNameFile "File> "
 s <- readFile f
 putStr "N> "
 n <- getLine
 putStr (printTail (findTail (lines s) n 0))

-- Задача 3 ----------------------------------------- 
digits :: Char -> Bool
digits c  = if (c=='1')||(c=='2')||(c=='3')||(c=='4')||(c=='5')||(c=='6')||(c=='7')||(c=='8')||(c=='9')|| (c=='0')||(c==' ')||(c=='-')||(c=='+')
 then True
 else False

check:: String -> Bool
check "" = True
check s = if (digits (head s)) then check (tail s)
 else False

delP:: String -> String
delP "" = ""
delP (s:xs)  = if (s=='+') then delP xs else s:delP xs

signs::String->Int->Int->Bool
signs "" c a = if (c<=a) then True else False
signs (s:xs) c a = if (s=='+')||(s=='-') 
 then if (c>a) then False
 else signs xs (c+1) a
 else signs xs c a

findInts:: String -> String -> String
findInts s s1 = if ((check s)&&(check s1))&&(length (words s)==1)&&(length (words  s1)==1)&&(signs s 0 1)&&(signs s1 0 1)
 then show ((read (delP s) :: Int)  + (read (delP s1) :: Int))
 else "error"

sum2 :: IO()
sum2 = do
 s <- getLine
 s1 <- getLine
 putStrLn (findInts s s1)

-- Задача 4 -----------------------------------------
checkD::String -> Bool
checkD "" = True
checkD s = if (digits (head s))||(head s)=='.' then checkD (tail s)
 else False

parse:: String -> (Float,Float,Float)
parse "" = (0,0,0)
parse s = (a,b,c)
 where
 xs = words s
 a = read (head xs) :: Float
 b = read (head (tail xs)) :: Float
 c = read (last xs) :: Float

solve:: (Float, Float, Float) -> String
solve (0,0,0) = "many"
solve (a,b,c) = if (a==0) then "error" else
 if d==0 then ("x = " ++ show(x1)) else
 if d < 0 then "no" 
 else ("x1 = " ++ show (x1) ++ " x2 = " ++ show (x2))
 where
 x1 = e + sqrt d / (2 * a)
 x2 = e - sqrt d / (2 * a)
 d = b * b - 4 * a * c
 e = - b / (2 * a)
 
qEquation:: String -> String
qEquation "" = ""
qEquation s = if (checkD s)&&(length(words s)==3)&&(signs s 0 3) then solve(parse (delP s))
 else "error"

equation :: IO()
equation = do
 s <- getLine
 putStrLn (qEquation s)

-- Задача 5 ----------------------------------------- 
findBalance:: String -> String -> String
findBalance "" "" = "Is Balanced"
findBalance "" _ = "No Balanced"
findBalance (s:xs) ys = if (s=='(') then findBalance xs (s:ys)
 else if (s==')') then  
 if ((null ys)==False) then findBalance xs (tail ys) else "No balanced"
 else findBalance xs ys

balance :: IO()
balance = do
 f <- getNameFile "File > "
 s <- readFile f
 putStrLn (findBalance s "")