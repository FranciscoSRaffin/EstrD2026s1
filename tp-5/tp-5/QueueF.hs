module QueueF
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data Queue a = Q [a]

emptyQ :: Queue a                   -- O(1)
isEmptyQ :: Queue a -> Bool         -- O(1)
enqueue :: a -> Queue a -> Queue a  -- O(n) Siendo n la cantidad de elementos
dequeue :: Queue a -> Queue a       -- O(1)
firstQ :: Queue a -> a --PARCIAL    -- O(1)

emptyQ = (Q [])
isEmptyQ (Q xs) = isEmptyL xs
enqueue x (Q xs) = (Q (xs ++ [x]))  
dequeue (Q xs) = (Q (sinPrimero xs)) 
firstQ (Q xs) = firstL xs 

isEmptyL :: [a] -> Bool
isEmptyL a:_ = False
isEmptyL _   = True

sinPrimero :: [a] -> [a]
sinPrimero (x:xs) = xs
sinPrimero _      = []

-- PRECOND: La lista no debe estar vacía 
firstL :: [a] -> a
firstL (x:xs) = x
firstL _      = error "La cola esta vacia" 


