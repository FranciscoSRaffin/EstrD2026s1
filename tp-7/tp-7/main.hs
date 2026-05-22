import Empresa

-- 1
heapsort :: Ord a => [a] -> [a]
-- El costo es <n log n> ya que, al usar un heap como representaciòn (costo de recorrigo logarimico)
-- se realizan dos acciones logatìtmicas (inserciòn y borrado)
-- Insert y borrado -> Costo log(n)

-- 2

-- Costo: O(log N) -> O(1) + O(1) + O(log N) = O(log N)
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST _ EmptyT         = False -- O(1)
belongsBST e (NodeT x t1 t2) = (e == x) || if (e < x) -- O(1)  
                                            then belongsBST e t1 -- | Recorre UNA rama del arbol, por lo que su costo es O(log N)  
                                            else belongsBST e t2 -- |

-- Costo: O(log N) -> O(1) + O(log n) = O(log n)
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST e EmptyT         = (NodeT e EmptyT EmptyT) 
insertBST e (NodeT x t1 t2) = if (e < x) -- O(1)  
                                then insertBST e t1 -- | Recorre UNA rama del arbol, por lo que su costo es O(log n)  
                                else insertBST e t2 -- |

-- Costo: O(log N) consultar https://vscode.dev/github/FranciscoSRaffin/EstrD2026s1/blob/main/tp-7/ejemplos-clase-7/Set.hs#L59 y rever clase de bst
-- https://youtu.be/aSQmW__iWpc?list=PL6lbGW3UNWzwDvcKTCyGlNBeEtHvdKzKk&t=5581
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST e EmptyT         = EmptyT
deleteBST e (NodeT x t1 t2) =
            if (e == x)      then rearmarBST t1 t2
            else if ( e < x) then NodeT y (deleteBST e t1) t2 -- | Recorre UNA rama del arbol, por lo que su costo es O(log N)  
                             else NodeT y t1 (deleteBST e t2) -- |

-- Para rearmar un bst luego de haber eliminado un elemento, debo reemplazarlo con el mas grande de la izquierda o el mas chico de la derecha
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST t1 t2 = let (x, t1') = splitMaxBST t1
                    in NodeT x t1' t2

-- Devuelve el elemento mas grande del bst (el que se encuentra abajo a la derecha) junto con el bst sin ese elemento
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
  -- PRECOND: el árbol es BST, y NO está vacío
splitMaxBST (Node x t1 EmptyT) = (x, t1)
splitMaxBST (Node x t1 t2)    = let (m, t2') = splitMaxBST t2
                                in (m, Node x t1 t2')

-- Costo: O(log N)
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST (NodeT x EmptyT t2) = (x, t2)
splitMinBST (NodeT x t1 t2)     = let (m, t1') = splitMinBST t1
                                    in (m, NodeT x t1' t2)

--Costo: O(N2) -> Por cada elemento se observan todos los elementos
esBST :: Ord a => Tree a -> Bool
esBST EmptyT          = True
esBST (NodeT x t1 t2) = esMayorATodos x t1 && esMenorATodos x t2 && esBST t1 && esBST t2

esMayorATodos :: Ord a => a -> Tree a -> Bool
esMayorATodos _ EmptyT          = True
esMayorATodos e (NodeT x t1 t2) = e > x && esMayorATodos e t1 && esMayorATodos e t2

esMenorATodos :: Ord a => a -> Tree a -> Bool
esMenorATodos _ EmptyT          = True
esMenorATodos e (NodeT x t1 t2) = e < x && esMenorATodos e t1 && esMenorATodos e t2

-- Costo: O(log N)
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA _ EmptyT          = Nothing
elMaximoMenorA k (NodeT x t1 t2) = case elMaximoMenorA k t2 of
                                Just g = if (k > g) then g else if (k > x) then x else Nothing   
                                Nothing if (k > x) then x else Nothing
-- Costo: O(log N)
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA _ EmptyT          = Nothing
elMinimoMayorA k (NodeT x t1 t2) = case elMinimoMayorA k t2 of
                                Just g  = if (k < g) then g else if (k < x) then x else Nothing   
                                Nothing =if (k < x) then x else Nothing
--Costo: O(N2)
balanceado :: Tree a -> Bool
balanceado tree = (heightT tree - shorterHeightT tree) >= 1

shorterHeightT :: Tree a -> Int
shorterHeightT EmptyT          = 0
shorterHeightT (NodeT _ t1 t2) = 1 + min (heightT t1) (heightT t2)

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

-- 3

-- TODO COSTOS

-- Costo: O(1)
-- emptyM :: Map k v
-- Costo: O(log K)
-- assocM :: Ord k => k-> v-> Map k v-> Map k v
-- Costo: O(log K)
-- lookupM :: Ord k => k-> Map k v-> Maybe v
-- Costo: O(log K)
-- deleteM :: Ord k => k-> Map k v-> Map k v
-- Costo: O(K)
-- keys :: Map k v-> [k]


-- 5
-- TODO COSTOS
comenzarCon :: [SectorId] -> [CUIL] -> Empresa
comenzarCon sIds cuils =  agregarSectores sIds (agregarEmpleado sids consEmpresa)

agregarSectores :: [SectorId] -> Empresa -> Empresa
agregarSectores []     empresa = empresa 
agregarSectores (s:ss) empresa = agregarSector s (agregarSectores ss empresa)

recorteDePersonal :: Empresa -> Empresa
recorteDePersonal empresa = eliminarEmpleados (descartarMitad (todosLosCUIL empresa)) empresa


eliminarEmpleados :: [CUIL] -> Empresa -> Empresa
eliminarEmpleados []     empresa = empresa
eliminarEmpleados (c:cs) empresa = borrarEmpleado c (eliminarEmpleados cs empresa)

descartarMitad :: [a] -> [a]
descartarMitad lista = descartarMitad' True lista

descartarMitad' :: Bool -> [a] -> [a]
descartarMitad' _        []     = []
descartarMitad' delFirst (x:xs) = if (delFirst)
                                then descartarMitad' not delFirst xs
                                else x : descartarMitad' not delFirst xs

convertirEnComodin :: CUIL -> Empresa -> Empresa
convertirEnComodin cuil empresa = asignarA (todosLosSectores empresa) cuil empresa 

asignarA :: [SectorId] -> CUIL -> Empresa -> Empresa
asignarA []     _    empresa = empresa 
asignarA (s:ss) cuil empresa = agregarASector s cuil (asignarA ss empresa)

esComodin :: CUIL -> Empresa -> Bool
esComodin cuil empresa = lenght sectores (buscarPorCUIL cuil empresa) == lenght todosLosSectores empresa
-- Dado que el empleado pertenece unicamente a sectores de la empresa, el tamaño de estas dos listas es comparable
