import PriorityQueue
import MultiSet
import Map

-- 2

-- O(n) n = cant.elementos 
heapSort :: Ord a => [a] -> [a]
heapSort xs = pqToList (listToPQ xs)

listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ (x:xs) = insertPQ x (listToPQ xs) 
listToPQ _      = emptyPQ

pqToList :: Ord a => PriorityQueue a -> [a] 
pqToList pq = if (isEmptyPQ pq)
                then []
                else (findMinPQ pq) : (pqToList (deleteMinPQ pq)) 

-- 3 Map

-- 3.1
-- NOTA: Recorro los valores en lugar de recorrer el map ya que
-- al no tener la funcion EmptyM, tendria que evaluar sobre keys 
-- por cada elemento lo que haria un coste de O(n^n)
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = values (keys map) map 

values :: Eq k => [k] -> Map k v -> [Maybe v]
values (k:ks) map = (lookupM k map) : (values ks map)
values _      _   = []

-- 3.2
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas (k:ks) map = (isJust (lookupM k map)) && todasAsociadas ks map

isJust :: Maybe a -> Bool
isJust (Just _) = True 
isJust _        = False

-- 3.3
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap ((k,v):kvs) map = assocM k v (listToMap kvs map)
listToMap _           _   = emptyM

-- 3.4
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = mapToListK (keysM map) map

mapToListK :: Eq k => [k] -> Map k v -> [(k, v)]
mapToListK (k:ks) map = (k, (lookupM k map)) : mapToListK ks map
mapToListK _      _   = []

-- 3.5
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq ((k,v):kvs) = case lookupM k (agruparEq kvs) of 
        Nothing -> assocM k [v]
        just vs -> assocM k (v:vs)
agruparEq _ = emptyM 

-- 3.6
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar (k:kvs) map = case lookupM k map of
        Nothing -> error "Uno de las claves contenidas dentro e la lista dada no se encuentra asociada a ningun numero dentro del map"
        Just n  -> assocM k (n+1) map
incrementar _       map = map

-- 3.7
mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = assocValues (mapToList map1) map2

assocValues :: Eq k => [(k, v)] -> Map k v -> Map k v
assocValues ((k, v):kvs) map = assocM k v (assocValues kvs map)
assocValues _            map = map

-- 5.1 
indexar :: [a] -> Map Int a
indexar as = indexar' (cantidadDeElementos as) as

indexar' :: [a] -> Int -> Map Int a
indexar' (x:xs) n = assocM n x (indexar' (n-1) xs) 
indexar' _ _ = emptyM

-- 5.2
ocurrencias :: String -> Map Char Int
ocurrencias (c:cs) = let restoDeOcurrencias = ocurrencias cs in assocM c (justOrCero (lookupM c restoDeOcurrencias) + 1) (restoDeOcurrencias)
ocurrencias []     = emptyM

justOrCero :: Maybe Int -> Int
justOrCero (Just x) = x
justOrCero Nothing = 0

-- TODO COSTOS

-- TODO 5.3.1

-- 5.3
ocurrencias' :: String -> MultiSet Char Int
ocurrencias' (c:cs) = addMS c (ocurrencias cs)
ocurrencias' []     = emptyMS