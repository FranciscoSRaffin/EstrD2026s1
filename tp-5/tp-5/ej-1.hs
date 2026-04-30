-- Cálculo de costos

-- 1. Especificar el costo operacional de las siguientes funciones:
head' :: [a] -> a -- > O(1) <Constante> 
head' (x:xs) = x   

sumar :: Int -> Int -- > O(1) <Constante>  
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

factorial :: Int -> Int -- > O(n) <Lineal> -> <n> es el numero introducido  
factorial 0 = 1
factorial n = n * factorial (n-1)

longitud :: [a] -> Int -- > O(n) <Lineal> -> <n> es el largo de la lista
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

factoriales :: [Int] -> [Int] -- > O(n*y) <Cuadratico> -> <n> es el numero introducido e <y> la cantidad de elementos de la lista 
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

pertenece :: Eq a => a -> [a] -> Bool -- > O(n) <Lineal> -> <n> es el largo de la lista
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

sinRepetidos :: Eq a => [a] -> [a] -- > O(n) <Lineal> -> <n> es el largo de la lista
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs then sinRepetidos xs else x : sinRepetidos xs

append :: [a]-> [a] -> [a] -- > O(n) <Lineal> -> <n> es el largo de la primera lista
append [] ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String] -> String -- > O(1) <Constante>
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int-> [a] -> [a] -- > O(y-n) <Lineal> -> <n> es el largo de la lista e <y> el numero introducido 
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a] -- > O(y-n) <Lineal> -> <n> es el largo de la lista e <y> el numero introducido
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

partir :: Int-> [a] -> ([a], [a]) -- > O(n) <Lineal> -> <n> es el largo de la lista
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a  -- > O(n) <Lineal> -> <n> es el largo de la lista
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a-> [a] -> [a]  -- > O(n) <Lineal> -> <n> es el largo de la lista
sacar n [] = []
sacar n (x:xs) = if n == x then xs else x : sacar n xs

ordenar :: Ord a => [a] -> [a]  -- > O(n²) <Cuadratico> -> <n> es el largo de la lista
ordenar [] = []
orderar xs = let m = minimo xs in m : ordenar (sacar m xs)


