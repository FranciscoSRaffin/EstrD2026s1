
delta :: Bool -> Int  -- Función delta de Kröenecker (unoSiCeroSino)
delta True  = 1
delta False = 0

singularSi :: a -> Bool -> [a]
singularSi x True  = x:[]
singularSi x False = []

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

cantidadDeElementos :: [a] -> Int
cantidadDeElementos []   = 0
cantidadDeElementos (a:as) = 1 + cantidadDeElementos as