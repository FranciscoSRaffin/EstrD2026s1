import  Set 

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

--2.3
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT            = emptyS 
unirTodos (NodeT set b1 b2) = unionS set (unionS (unirTodos b1) (unirTodos b2)) 

