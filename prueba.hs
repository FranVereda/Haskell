division :: Int -> Int -> Int
division  = div

rec :: [a] -> Int
rec [] = 0
rec (x:xs) = 1 + rec xs

contarel :: Eq a => a -> [a] -> Int
contarel elem [] = 0
contarel elem (x:xs)
    | elem == x = 1 + contarel elem xs
    | otherwise = contarel elem xs

pert :: Eq a => a -> [a] -> Bool
pert _ [] = False
pert elem (x:xs)
    | elem == x = True
    | otherwise = pert elem xs

dupp :: [a] -> [a]
dupp [] = []
dupp (x:xs) = x : x : dupp xs

cant :: [a] -> [a] -> Int
cant [] l2 =2
cant l1 [] =1
cant [a] [b] = 0
cant (x:xs) (a:ad) = cant xs ad 

fact :: [Float] -> Float
fact [] = 1
fact (x:xs) = x * fact xs

sumasS :: [Int] -> Int
sumasS [] = 0
sumasS [a,b] = if a == 0 then 0
else b + sumasS [a-1,b]

reve :: [a] -> [a]
reve [] = []
reve (x:xs) = reve xs ++ [x]

reveL :: [[a]] -> [[a]]
reveL [] = []
reveL (x:xs) = reve x : reveL xs

ordA :: [a] -> [a]
ordA [] = []
ordA (x:xs) = x : ordA xs ++ [x]

ordB :: [a] -> [a]
ordB [] = []
ordB (x:xs) = x : x : ordB xs

nPar :: Int -> [Int]
nPar 0 = []
nPar n =  nPar (n-1) ++ [2*n]

nMul :: Int -> Int -> [Int]
nMul 0 x = []
nMul n x =  nMul (n-1) x ++ [n*x]

lenAcc :: [a] -> Int
lenAcc (x:xs) = aux 0 (x:xs) where
    aux acc [] = acc
    aux acc (x:xs) = aux (acc + 1) xs

canV :: Eq a => [a] -> a -> Int
canV (x:xs) elem = aux 0 elem (x:xs) where
    aux acc elem [] = acc
    aux acc elem (x:xs) 
        | elem == x = aux (acc + 1) elem xs
        | otherwise = aux acc elem xs

canB :: Eq a => [a] -> a -> Bool
canB (x:xs) elem = aux False elem (x:xs) where
    aux acum elem [] = acum
    aux acum elem (x:xs) 
        | elem == x = aux True elem xs
        | otherwise = aux acum elem xs

duppe :: [a] -> [a]
duppe (x:xs) = aux [] (x:xs) where
    aux acc [] = acc
    aux acc (x:xs) = aux (acc ++[x, x]  ) xs 

duppeA :: [a] -> [a]
duppeA (x:xs) = aux [] (x:xs) where
    aux acc [] = acc
    aux acc (x:xs) = aux ( acc ++ x : [x]) xs

duppeB :: [a] -> [a]
duppeB (x:xs) = aux [] (x:xs) where
    aux acc [] = acc
    aux acc (x:xs) = aux (x : acc ++ [x]) xs

duppeC :: [a] -> [a]
duppeC (x:xs) = aux [] (x:xs) where
    aux acc [] = acc ++ acc
    aux acc (x:xs) = aux ((acc ++ [x])) xs

mom :: [a] -> [a] -> Int
mom (x:xs) (d:ds) = aux 0 (x:xs) (d:ds) where
    aux acc [] [] = acc
    aux acc [] _ = 2
    aux acc _ [] = 1
    aux acc (x:xs) (d:ds) = aux acc xs ds

sumRe :: [Int] -> Int
sumRe [a,b] = aux 0 [a,b] where
    aux acc [0,0] = acc
    aux acc [0,b] = acc
    aux acc [a,0] = acc
    aux acc [a,b] = aux (acc+b) [a-1, b]

revv :: [a] -> [a]
revv (x:xs) = aux [] (x:xs) where
    aux acc [] = acc
    aux acc (x:xs) = aux (x : acc  ) xs

revvA :: [[a]] -> [[a]]
revvA (x:xs) = aux [] (x:xs) where
    aux acc [] = acc
    aux acc (x:xs) = aux (acc ++ [revv x] ) xs

nNa :: Int -> [Int]
nNa num = aux [] num where
    aux acc 0 = acc
    aux acc num = aux ([2*num] ++ acc) (num-1)

sumsuc :: Int -> Int -> Int
sumsuc num1 num2 = aux 0 num1 num2 where
    aux acc 0 num2 = acc
    aux acc num1 0  = acc
    aux acc num1 num2 = aux (acc + num1) num1 (num2-1)

--foldr foldr1 foldl foldl1

ffra :: [a] -> Int
ffra lista = foldr (\_ acc -> acc + 1) 0 lista

ffrb :: Eq a => a -> [a] -> Int
ffrb elem lista = foldr (\x acc -> if x == elem then acc + 1 else acc) 0 lista

ffrc :: Eq a => a -> [a] -> Bool
ffrc elem lista = foldr (\x acc -> if x == elem then True else acc) False lista

ffrd :: [a] -> [a]
ffrd lista = foldr (\x acc -> x : x : acc ) [] lista

ffrf :: [Float] -> Float
ffrf lista = foldr (\x acc -> if x == 0 then 0 else ((acc) * x)) 1 lista 

ffrg :: [Int] -> Int
ffrg [a,b] = foldr (+) 0 (replicate b a)  

ffrh :: [a] -> [a]
ffrh lista = foldr (\x acc -> acc ++ [x]) [] lista

ffri :: [[a]] -> [[a]]
ffri lista = foldr (\x acc -> ffrh x : acc ) [] lista

ffla :: [a] -> Int
ffla lista = foldl (\acc x -> acc + 1) 0 lista

fflb :: Eq a => a -> [a] -> Int
fflb elem lista = foldl (\acc x -> if x == elem then acc+1 else acc) 0 lista

fflc :: Eq a => a -> [a] -> Bool
fflc elem lista = foldl (\acc x -> if x == elem then True else acc) False lista

ffld :: [a] -> [a]
ffld lista = foldl (\acc x -> [x] ++ [x] ++ acc ) [] lista

ffld2 :: [a] -> [a]
ffld2 lista = foldl (\acc x -> acc ++ [x, x] ) [] lista

fflf :: [Float] -> Float
fflf lista = foldl (\acc x -> acc*x) 1 lista

fflg :: [Int] -> Int
fflg [a,b] = foldl (+) 0 (replicate a b)

fflh :: [a] -> [a]
fflh lista = foldl (\acc x -> x : acc) [] lista

--foldr1

fr1a :: [a] -> Int
fr1a = foldr1 (\_ acc -> acc + 1) . map (const 1)

fr1c :: Eq a => a -> [a] -> Bool 
fr1c elem lista = foldr1 (||) (map (== elem) lista)

fr1f :: [Float] -> Float
fr1f lista  = foldr1 (\x acc -> acc * x) lista

fr1g :: [Int] -> Int
fr1g [a,b] = foldr1 (+) (replicate b a)

fff :: [String] -> String
fff = foldr1 (\x acc -> x ++ " " ++ acc)





