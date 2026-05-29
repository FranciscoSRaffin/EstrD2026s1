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
--Precondición: El tripulante y el sector existen

construir sIds = N (crearSectores sIds) emptyM emptyM

crearSectores :: [SectorId] -> Map SectorId Sector
crearSectores []     = emptyM
crearSectores (s:ss) = assocM s (crearS s) (crearSectores ss)

ingresarT nombre rango (N sectores nombres rango) = let tripulante = crearT nombre rango
                                             in case lookupM nombre nombres of 
                                                Just _ = error "El tripulante ya se encuentra en la nave"
                                                Nothing = (N sectores (assocM nombre tripulante nombres) (insertH tripulante rango))

sectoresAsignados nombre (N sectores nombres _) = case lookupM nombre nombres of 
                                                Nothing = error "No existe tripulante con ese nombre"
                                                Just _ = sectoresAsignadosPrima (domM sectores) sectores nombre

sectoresAsignadosPrima :: [SectorId] -> Map SectorId Sector -> String -> Set SectorId 
sectoresAsignadosPrima []     _        _      = emptyS
sectoresAsignadosPrima (s:ss) sectores nombre = if (belongsS nombre tripulantesS lookupM sectores)
                                                then addS nombre sectoresAsignadosPrima ss sectores nombre
                                                else sectoresAsignadosPrima ss sectores nombre

datosDeSector sid (N sectores _ _) = case lookupM sid sectores of
    Nothing     = error "El sector buscado no se encuentra en la nave" 
    Just sector = (tripulantesS sector, componentesS sector)

tripulantesN (N _ nombres _) = valuesOf (domM nombres) nombres 

valuesOf :: [String] -> Map k v -> [v]
valuesOf []     _   = []
valuesOf (k:ks) map = lookupM k map : valuesOf ks map

agregarASector cs sid (N sectores nombres rango) -> (N (agregarASectorPrima cs sid sectores) nombres rango)

agregarASectorPrima cs sid sectores = let sector = lookupM sid sectores in assocM sid (agregarComponentes cs sector) sectores 

agregarComponentes :: [Componente] -> Sector -> Sector
agregarComponentes []     sector = sector
agregarComponentes (c:cs) sector = agregarC c (agregarComponentes cs sector)

asignarASector nombre sid (N sectores nombres rango) = case lookupM nombre nombres of
                                    Nothing         = error "El tripulante no se encuentra en la nave"
                                    Just tripulante = (N (asignarASectorPrima nombre sid sectores) nombres rango) 

asignarASectorPrima :: String -> SectorId -> Map SectorId Sector 
asignarASectorPrima nombre sid sectores = case lookupM sid sectores of
                                    Nothing     = error "El sector no se encuentra en la nave"
                                    Just sector = assocM sid (agregarT nombre sector) sectores