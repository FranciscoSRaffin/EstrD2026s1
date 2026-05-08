module QueueCons
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data Queue a = Q [a] [a]
  {- INV.REP.: en (Q [a] [a])
      * TODO
  -}

emptyQ :: Queue a                   -- O(1)
isEmptyQ :: Queue a -> Bool         -- O(1)
enqueue :: a -> Queue a -> Queue a  -- O(1)
dequeue :: Queue a -> Queue a       -- O(1) ?????
firstQ :: Queue a -> a --PARCIAL    -- O(1)

emptyQ = (Q [] [])
isEmptyQ (Q fs _) = isEmptyL fs
enqueue x (Q fs bs) = (Q fs (x:bs))  
dequeue (Q fs bs) = (Q 
            (if isEmptyL fs then (reverseL bs) else (sinPrimero fs))
            (if isEmptyL fs then [] else bs)
        ) 
firstQ (Q fs _) = firstL fs 

reverseL :: [a] -> [a]
reverseL (a:as) = agregarAlFinal (reverseL as) a
reverseL _      = []

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal (a:as) x = a : agregarAlFinal as x
agregarAlFinal _      x = x:[]

isEmptyL :: [a] -> Bool
isEmptyL (a:_) = False
isEmptyL _     = True

sinPrimero :: [a] -> [a]
sinPrimero (x:xs) = xs
sinPrimero _      = []

-- PRECOND: La lista no debe estar vacía 
firstL :: [a] -> a
firstL (x:xs) = x
firstL _      = error "La cola esta vacia" 


