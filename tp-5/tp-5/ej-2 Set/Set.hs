module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
    where

data Set a = S [a]

-- 2.1
emptyS :: Set a
emptyS = (S [])

addS :: Eq a => a -> Set a -> Set a
addS e (S es) = S (singularSi e (not (pertenece e es)) ++ es)

belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = pertenece x xs

sizeS :: Eq a => Set a -> Int
sizeS (S xs) = longitud xs

removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs) = S (removeL x xs)

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (sinRepetidos xs ++ ys) 

setToList :: Eq a => Set a -> [a]
setToList (S xs) = xs

--- ================= ---

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs then sinRepetidos xs else x : sinRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys


longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


removeL :: Eq a => a -> [a] -> [a] 
removeL _ []     = []
removeL x (y:ys) = if (x == y) then ys else y:(removeL x ys)

singularSi :: a -> Bool -> [a]
singularSi x True  = x:[]
singularSi x False = []