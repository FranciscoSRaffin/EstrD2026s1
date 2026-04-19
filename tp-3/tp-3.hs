unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _    = 0

singularSi :: a -> Bool -> [a]
singularSi x True  = x:[]
singularSi x False = []

-- TIPOS RECURSIVOS SIMPLES

-- 1 Celdas con bolitas

data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

-- 1.1
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia  = 0
nroBolitas c (Bolita co ce) = (unoSiCeroSino (sonMismoColor c co)) + nroBolitas co ce

sonMismoColor :: Color -> Color -> Bool
sonMismoColor Azul Azul = True
sonMismoColor Rojo Rojo = True
sonMismoColor _    _    = False

-- 1.2
poner :: Color -> Celda -> Celda
poner co c = (Bolita co c)

-- 1.3
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia    = CeldaVacia
sacar c1 (Bolita c ce) = if (sonMismoColor c1 c)
                          then ce
                          else (Bolita c (sacar c1 ce))

-- 1.4
ponerN :: Int -> Color -> Celda -> Celda
ponerN n co ce = if (n > 0)
    then (Bolita co (ponerN (n-1) co ce))
    else ce

-- 2 Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-- 2.1.1
hayTesoro :: Camino -> Bool
hayTesoro (Cofre co ca) = (hayTesoroCofre co) || (hayTesoro ca)
hayTesoro (Nada ca) =  hayTesoro ca
hayTesoro _ = False

hayTesoroCofre :: [Objeto] -> Bool
hayTesoroCofre []   = False
hayTesoroCofre (o:os) = esTesoro o || hayTesoroCofre os

-- 2.1.2
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Nada ca) = 1 + pasosHastaTesoro ca
pasosHastaTesoro (Cofre co ca) = if (hayTesoroCofre co)
    then 0
    else 1 + pasosHastaTesoro ca
pasosHastaTesoro _ = 0

-- 2.1.3
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn p (Cofre co ca) = (p == 0 && hayTesoroCofre co) || hayTesoroEn (p-1) ca
hayTesoroEn p (Nada ca)     = p > 0 && hayTesoroEn (p-1) ca
hayTesoroEn _ _             = False

-- 2.1.4
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros cant (Nada ca)     = cant == 0 || alMenosNTesoros cant ca  
alMenosNTesoros cant (Cofre co ca) = (cant - cantidadDeTesoros co) <= 0 || (alMenosNTesoros (cant - cantidadDeTesoros co) ca) 
alMenosNTesoros cant _             = cant == 0

cantidadDeTesoros :: [Objeto] -> Int
cantidadDeTesoros []   = 0
cantidadDeTesoros (o:os) = unoSiCeroSino(esTesoro o) + cantidadDeTesoros os

-- TIPOS DE ARBOLES

-- 2 Arboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

esEmptyT :: Tree a -> Bool 
esEmptyT EmptyT = True
esEmptyT _      = False

contenido :: Tree a -> a
contenido EmptyT          = error "El arbol no puede ser vacio" 
contenido (NodeT n t1 t2) = n 

-- 2.1.1
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- 2.1.2
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2 

-- 2.1.3
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT 
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2))

-- 2.1.4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT          = False
perteneceT a (NodeT b t1 t2) = (a == b) || (perteneceT a t1) || (perteneceT a t2) 

-- 2.1.5
aparicionesT :: Eq a => a-> Tree a-> Int
aparicionesT _ EmptyT          = 0
aparicionesT a (NodeT b t1 t2) = unoSiCeroSino (a == b) + aparicionesT a t1 + aparicionesT a t2

-- 2.1.6
leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT b t1 t2) = (singularSi b (esEmptyT t1 && esEmptyT t2)) ++ leaves t1 ++ leaves t2

-- 2.1.7 
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ t1 t2) = max (length (leaves t1)) (length (leaves t2))

-- 2.1.8 
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT a t1 t2) = (NodeT a (mirrorT t2) (mirrorT t1))

-- 2.1.9 
toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT a t1 t2) = (singularSi (contenido t1) (not (esEmptyT t1))) ++ [a] ++ (singularSi (contenido t2) (not (esEmptyT t2))) 

-- 2.1.10
levelN :: Int-> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT a _ _ )  = a : []
levelN l (NodeT _ t1 t2) = levelN (l-1) t1 ++ levelN (l-1) t2

-- 2.1.11 
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : zipNodos (listPerLevel t1)  (listPerLevel t2) 

zipNodos :: [[a]] -> [[a]] -> [[a]]
zipNodos []     yss        = yss
zipNodos xss    []         = xss
zipNodos (xs:xss) (ys:yss) = (xs ++ ys) : zipNodos xss yss

-- 2.1.12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : if (heightT t1 > heightT t2)
                                   then ramaMasLarga t1 
                                   else ramaMasLarga t2

-- 2.1.13
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = (consACada x (todosLosCaminos t1)) ++ (consACada x (todosLosCaminos t2))

consACada :: a -> [[a]] -> [[a]]
consACada y []       = [y:[]]
consACada y (xs:xss) = (y:xs):(consACada y xss)

-- 2.2 
-- Expresiones Aritméticas

data ExpA = Valor Int
          | Sum ExpA ExpA
          | Prod ExpA ExpA
          | Neg ExpA

-- 2.2.1
eval :: ExpA -> Int
eval (Valor n)    = n
eval (Sum e1 e2)  = (eval e1) + (eval e2)
eval (Prod e1 e2) = (eval e1) * (eval e2)
eval (Neg e)      = - (eval e)

-- 2.2.2
simplificar :: ExpA -> ExpA
simplificar (Sum e1 e2)  = simplificarSum (eval e1) (eval e2)
simplificar (Prod e1 e2) = simplificarProd (eval e1) (eval e2)
simplificar (Neg e)      = simplificarNeg e
simplificar e = e

simplificarSum :: Int -> Int -> ExpA
simplificarSum 0 y = (Valor y) 
simplificarSum x 0 = (Valor x)
simplificarSum x y = (Sum (Valor x) (Valor y))

simplificarProd :: Int -> Int -> ExpA
simplificarProd 0 _ = (Valor 0)
simplificarProd _ 0 = (Valor 0)
simplificarProd 1 y = (Valor y)
simplificarProd x 1 = (Valor x)
simplificarProd x y = (Prod (Valor x) (Valor y))

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg x) = x
simplificarNeg e       = e