{-# OPTIONS_GHC -Wall #-}
module Polikarpova04 where

data Expression = Var String                  -- Змінна
               | Val Int                      -- Ціла константа
               | Op Expression Bop Expression -- Операція
                 deriving (Show, Eq)

-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Divide   
          | Gt | Ge | Lt | Le| Eql
            deriving (Show, Eq)

data Statement = Assign String Expression
               | Incr String
               | If Expression Statement Statement
               | While Expression Statement       
               | For Statement Expression Statement Statement
               | Sequence Statement Statement        
               | Skip
                 deriving (Show, Eq)

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

type State = [(String, Int)]

-- Задача 1 -----------------------------------------
get ::  State -> String -> Int
get [] _ = 0
get ((k,v):st) x = if (k==x) then v else get st x
  
-- Задача 2 ----------------------------------------- 
extend :: State -> String -> Int -> State
extend s "" _ =  s
extend [] k v = [(k,v)] 
extend ((k,v):st) x newV = if (k==x) then (k,newV):st else  (k,v):extend st x newV

-- Задача 3 -----------------------------------------
evalE :: State -> Expression -> Int
evalE st (Var x) = get st x
evalE _ (Val i) = i

evalE _ (Op (Val x) o (Val y)) = case o of
 Plus -> x + y
 Minus -> x - y
 Times -> x * y
 Divide -> x `div` y
 Gt -> if (x > y) then 1 else 0
 Ge -> if (x >= y) then 1 else 0
 Lt -> if (x < y) then 1 else 0
 Le -> if (x <= y) then 1 else 0
 Eql -> if (x == y) then 1 else 0

evalE st (Op exp1 o exp2) = evalE st (Op (Val (evalE st exp1)) o  (Val (evalE st exp2))) 

-- Задача 4 -----------------------------------------
desugar :: Statement -> DietStatement
desugar Skip = DSkip
desugar (Assign s e) = DAssign s e
desugar (If e st1 st2) = DIf e (desugar st1) (desugar st2)
desugar (While e s) = DWhile e (desugar s)
desugar (Sequence st1 st2)  = DSequence (desugar st1)  (desugar  st2) 
desugar  (Incr s) = DAssign s (Op (Var s) Plus (Val 1)) 
desugar (For si e sn s) =  DSequence (desugar si) (DWhile e (DSequence (desugar s) (desugar sn)))

-- Задача 5 -----------------------------------------
evalSimple :: State -> DietStatement -> State  
evalSimple st (DAssign s e)= extend st s (evalE st e)
evalSimple st DSkip = st
evalSimple st (DIf e d1 d2) = if ((evalE st e) == 1) then evalSimple st d1
 else evalSimple st d2

evalSimple st (DWhile e d1) = if (evalE st e)==1 then 
 evalSimple (evalSimple st d1) (DWhile e d1)
 else evalSimple st DSkip

evalSimple st (DSequence d1 d2) =  (evalSimple (evalSimple st d1) d2)
 
-- Задача 6 -----------------------------------------
run :: State -> Statement -> State 
run st stm = evalSimple st (desugar stm)

-- Програми -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Обчислення значення b(6) в степені e(5) в змінній out 

   { b := 6; e := 5; out:= 1;
     for (i:=0; i<e; i++) out := out*b   
   }
-}

power :: Statement
power = slist [ Assign "b" (Val 6)
              , Assign "e" (Val 5) 
              , Assign "out" (Val 1)
              , For (Assign "i" (Val 0))
                    (Op (Var "i") Lt (Var "e"))
                    (Incr "i")
                    (Assign "out" (Op (Var "out") Times (Var "b")))
              ] 

{- Обчислення цілого значення корня квадратного 
   зі значення змінної a (317) в змінній b 

   {a := 317; b := 0;
    while (a >= b*b) b++;
    b := b-1
   } 	
-}
squareRoot :: Statement
squareRoot = slist [ Assign "a" (Val 317)
                   , Assign "b" (Val 0)
                   , While (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                       (Incr "b")
                   , Assign "b" (Op (Var "b") Minus (Val 1))
                   ]

{- Обчислює значення 12-го числа Фібонначі в змінній out

  {in := 12; f0 := 1; f1 := 1;
   if (in == 0) then out := f0 else 
     if (in == 1) then out := f1 else 
       for (c := 2; c < in; c++) {
         out := f0 + f1; f0 := f1; f1 := out
       }
  }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "in" (Val 12)
                  , Assign "f0" (Val 1)
                  , Assign "f1" (Val 1)
                  , If (Op (Var "in") Eql (Val 0))
                       (Assign "out" (Var "f0"))
                       (If (Op (Var "in") Eql (Val 1))
                           (Assign "out" (Var "f1"))
                           (For (Assign "c" (Val 2))
                                (Op (Var "c") Lt (Var "in"))
                                (Incr "c")
                                (slist
                                 [ Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                 , Assign "f0" (Var "f1")
                                 , Assign "f1" (Var "out")
                                 ])
                           )
                       )
                  ]

