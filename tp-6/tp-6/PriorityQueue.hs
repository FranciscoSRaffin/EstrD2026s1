module PriorityQueue 
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
    where

data PriorityQueue a = PQ [a]

emptyPQ :: PriorityQueue a                                   -- O(1)
isEmptyPQ :: PriorityQueue a -> Bool                         -- O(1)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a -- O(n) n = cant.elementos 
findMinPQ :: Ord a => PriorityQueue a -> a --PARC. (PQ != ∅) -- O(1)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a   -- O(1)

emptyPQ = PQ []
isEmptyPQ (PQ xs) = isEmptyL xs
insertPQ x (PQ xs) = (PQ (insertarOL x xs))
findMinPQ (PQ xs) = firstL xs
deleteMinPQ (PQ xs) = (PQ (sinPrimero xs))

isEmptyL :: [a] -> Bool
isEmptyL (a:_) = False
isEmptyL _     = True

insertarOL :: Ord a => a -> [a] -> [a]
insertarOL x (y:ys) = if ( x >= y) then x:y:ys else y:(insertarOL x ys)
insertarOL x _      = [x]

-- PRECOND: La lista no debe estar vacía 
firstL :: [a] -> a
firstL (x:xs) = x
firstL _      = error "La cola esta vacia" 

sinPrimero :: [a] -> [a]
sinPrimero (x:xs) = xs
sinPrimero _      = []