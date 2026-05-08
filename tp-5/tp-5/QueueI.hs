module QueueI
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data Queue a = Q [a]

emptyQ :: Queue a                   -- O(1)
isEmptyQ :: Queue a -> Bool         -- O(1)
enqueue :: a -> Queue a -> Queue a  -- O(1)
dequeue :: Queue a -> Queue a       -- O(n) Siendo n la cantidad de elementos
firstQ :: Queue a -> a --PARCIAL    -- O(n) Siendo n la cantidad de elementos

emptyQ = (Q [])
isEmptyQ (Q xs) = isEmptyL xs
enqueue x (Q xs) = (Q (x:xs))  
dequeue (Q xs) = (Q (sinUltimo xs)) 
firstQ (Q xs) = lastL xs -- PRECOND: La cola no debe estar vacía 

isEmptyL :: [a] -> Bool
isEmptyL a:_ = False
isEmptyL _   = True

sinUltimo :: [a] -> [a]
sinUltimo [x]     = []
sinUltimo (x:xs)  = x: sinUltimo xs
sinUltimo _       = []

lastL :: [a] -> a
lastL [x]    = x
lastL (x:xs) = lastL xs
lastL _      = error "La cola esta vacia" 


