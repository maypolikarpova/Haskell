module Set where
set:: [Int] -> [Int]
set xs = if null xs then xs else
 (head xs): set (filter (/=head xs) (tail xs))

listSum :: [Int]-> [Int] -> [Int]

listSum xs ys = if null xs then ys
 else if null ys then xs 
 else (head xs + head ys): listSum (tail xs) (tail ys)
