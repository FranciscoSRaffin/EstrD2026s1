import Set
import QueueCons
import Stack

-- DISCLAIMER: Esto nunca va a compilar porque la practica exige usar Stack y Set
--   y ambos se instancian con emptyS

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

-- sinRepetidos :: Eq a => [a] -> [a] -- > O(n) <Lineal> -> <n> es el largo de la lista
-- sinRepetidos [] = []
-- sinRepetidos (x:xs) = if pertenece x xs then sinRepetidos xs else x : sinRepetidos xs

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
ordenar xs = let m = minimo xs in m : ordenar (sacar m xs)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- 2.1
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     _   = []
losQuePertenecen (x:xs) set = singularSi x (belongs x set) ++ losQuePertenecen xs set

singularSi :: a -> Bool -> [a]
singularSi x True  = x:[]
singularSi x False = []

-- 2.2
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos list = setToList (listToSet list)

listToSet :: Eq a => [a] -> Set a
listToSet []     = emptyS
listToSet (x:xs) = addS x (listToSet xs)

-- 2.3
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT            = emptyS 
unirTodos (NodeT set b1 b2) = unionS set (unionS (unirTodos b1) (unirTodos b2)) 



-- 3.3 
-- Como usuario del tipo abstracto Queue implementar las siguientes funciones

lengthQ :: Queue a -> Int
lengthQ queue = if (isEmptyQ queue)
                    then 0
                    else 1 + lengthQ (dequeue queue)

queueToList :: Queue a -> [a]
queueToList queue = if (isEmptyQ queue)
                        then []
                        else (firstQ queue) : queueToList (dequeue queue)

unionQ :: Queue a -> Queue a -> Queue a
unionQ queueA queueB = if (isEmptyQ queueA)
                            then queueB
                            else unionQ (dequeue queueA) (enqueue (firstQ queueA) queueB)

-- 4.1
apilar :: [a] -> Stack a
apilar (x:xs) = push a (apilar xs)
apilar _      = emptyS

-- 4.2
desapilar :: Stack a -> [a]
desapilar stack = if (isEmptyS)
                        then []
                        else top stack : desapilar (pop stack)

-- 4.3
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos pos elem stack = if (pos == 0)
                            then push elem stack
                            else push (top stack) (insertarEnPos (pos-1) elem (pop stack))