module Nave (
    Nave,
    construir
    ingresarT
    sectoresAsignados
    datosDeSector
    tripulantesN
    agregarASector
    asignarASector
) where


data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

{- INV. REP:
    Dada la nave representada por (mapSectores sid sector) (mapAsignaciones nom tripV) (maxHeap tripH)

    - Todo tripulante (tripV), el cual se encuentra contenido en el segundo map, tiene como nombre el mismo
        nombre que se esta asignando como clave del mismo (nom) en dicho mapa.
    - Todo tripulante (tripV) contenido en el segundo map (mapAsignaciones), se encuentra a su vez en el maxHeap
    - Todo tripulante contenido en el maxHeap se encuentra contenido también en el mapAsignaciones
    - Too sector al cual esta asignado un tripulante del maxHeap, se encuentra contenido en el mapSectores
-}

construir :: [SectorId] -> Nave 
-- Eficiencia: O(S)
ingresarT :: Nombre -> Rango -> Nave -> Nave 
-- Eficiencia: O(logT)
sectoresAsignados :: Nombre -> Nave -> Set SectorId 
-- Precondición: Existe un tripulante con dicho nombre. 
-- Eficiencia: O(logM)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente]) 
--Precondición: Existe un sector con dicho id. 
--Eficiencia: O(logS)
tripulantesN :: Nave -> [Tripulante] 
--Eficiencia: O(logT)
agregarASector :: [Componente] -> SectorId -> Nave -> Nave 
--Eficiencia: O(C +logS), siendo C la cantidad de componentes dados.
asignarASector :: Nombre -> SectorId -> Nave -> Nave 
--Nota: No importa si el tripulante ya tiene asignado dicho sector. 
--Precondición: El tripulante y el sector e

construir sIds = N (crearSectores sIds) emptyM emptyM

crearSectores :: [SectorId] -> Map SectorId Sector
crearSectores []     = emptyM
crearSectores (s:ss) = assocM s (crearS s) (crearSectores ss)