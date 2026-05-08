module Stack
    (Stack, emptyS, isEmptyS, push, top, pop, lenS)
    where

data Stack a = S [a]

emptyS :: Stack a               -- O(1)
isEmptyS :: Stack a -> Bool     -- O(1)
push :: a -> Stack a -> Stack a -- O(1)
top :: Stack a -> a --PARCIAL   -- O(1)
pop :: Stack a -> Stack a       -- O(1)
lenS :: Stack a -> Int          -- O(n) Siendo n la cantidad de elementos

emptyS = S []
isEmptyS (S xs) = isEmptyL xs 
push x (S xs) = (S (x:xs)) 
top (S xs) = firstL xs
pop (S xs) = (S (sinPrimero xs))
lenS (S xs) = lenL xs

sinPrimero :: [a] -> [a]
sinPrimero (x:xs) = xs
sinPrimero _      = []

isEmptyL :: [a] -> Bool
isEmptyL (a:_) = False
isEmptyL   _   = True

lenL :: [a] -> Int
lenL (x:xs) = 1 + lenL xs
lenL _      = 0

-- PRECOND: El stack no debe estar vacía
firstL :: [a] -> a
firstL (x:xs) = x
firstL _      = error "El stack esta vacia" 

