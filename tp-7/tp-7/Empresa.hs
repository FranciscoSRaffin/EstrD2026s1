module Empresa ( Empresa,
    consEmpresa,
    buscarPorCUIL,
    empleadosDelSector,
    todosLosCUIL,
    todosLosSectores,
    agregarSector,
    agregarEmpleado,
    agregarASector,
    borrarEmpleado
    ) where

type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)


{- INV. REP:
 * Todo empleado que se encuentre en el set del prier map, debe estar contenido dentro del segundo map
    - EJ valido:   ConsE (assocM 12 (addS (consEmpleado 7276545619) (emptyS)) emptyM) (assocM 7276545619 (addS (consEmpleado 7276545619)
    - EJ invalido: ConsE (assocM 12 (addS (consEmpleado 7276545619) (emptyS)) emptyM) emptyM
 * Todo empleado contenido en el segundo map debe tener como clave el mismo cuil con el que fue creado
    - EJ valido:   ConsE (assocM 12 (addS (consEmpleado 7276545619) (emptyS)) emptyM) (assocM 7276545619 (addS (consEmpleado 7276545619)
    - EJ invalido: ConsE (assocM 12 (addS (consEmpleado 7276545619) (emptyS)) emptyM) (assocM 7276545619 (addS (consEmpleado 6626261523)
-}


consEmpresa :: Empresa
--Costo: O(1)

buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- Precondición: el CUIL es de un empleado de la empresa.
--Costo: O(log E)

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Costo: O(log S + E)

todosLosCUIL :: Empresa -> [CUIL]
--Costo: O(E)

todosLosSectores :: Empresa -> [SectorId]
--Costo: O(S)

agregarSector :: SectorId -> Empresa -> Empresa
--Costo: O(log S)

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Costo: calcular.

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Costo: calcular.

borrarEmpleado :: CUIL -> Empresa -> Empresa
--Costo: calcular.


consEmpresa = ConsE emptyM emptyM -- Costo O(1)


--                  /      O(1)      \          /      O(log E)     \
buscarPorCUIL cuil (ConsE _ empleados) = case lookupM cuil empleados of
                                                Nothing = error "El empleado no se encuentra en la empresas" -- O(1)
                                                Just em = em -- O(1)
-- COSTO FINAL: Costo O(log E) -> E cantidad de empleados



--                                             / O(E) \  / O(1) \ /      O(log S)      \    
empleadosDelSector id (ConsE asignaciones _) = setToList fromJust lookupM id asignaciones 
-- Suma: O(E) + O(1) + O(log S) = O(log S + E) 

-- COPY
-- (ConsE asignaciones empleados)  /\ O(1) O(E) O(S) O(log S) O(log E)

todosLosCUIL (ConsE _ empleados) = keysM empleados -- O(E)


todosLosSectores (ConsE asignaciones _) = keysM asignaciones -- O(S)

--                  /           O(1)          \          /      O(log S)       \
agregarSector sid (ConsE asignaciones empleados) = case lookupM sid asignaciones of
            Nothing = (ConsE (assocM sid emptyM asignaciones) empleados) -- O(1) + O(log S) 
            Just s  = error "La id introducida ya se encuentra asociada dentro de la empresa" -- O(1)
-- COSTO FINAL: O(log S) + O(log S) = O(log S)


agregarEmpleado []       _    empresa = empresa -- O(1)                                                | -- O(I*log S+E) 
agregarEmpleado (s:sids) cuil empresa = agregarEmpleado' s cuil (agregarEmpleado sids cuil empresa) -- |  I = cant. ids introducidas por parametro 
--               \O(1)/                 \---------------     O(log (S+E))   ---------------------------/  Por cada I se llama a agregarEmpleado' que es O(log S+E)
-- COSTO FINAL: O(I*log S+E) 
    -- Siendo
        -- I: Cantidad de ids de zonas introducidas por parametro  
        -- E: Cantidad de empleados en la empresa
        -- S: Cantidad de sectores en la empresa

-- NOTA: TODO: CONTEMPLAR Y ESPECIFICAR COSTO CONDICIONAL, ES DECIR "si el epleado existe el costo es A y si no existe es B"
agregarEmpleado' :: SectorId -> CUIL -> Empresa -> Empresa
agregarEmpleado' sid cuil (ConsE asignaciones empleados) = case lookupM cuil empleados of -- O(1)
                        Just em = (ConsE (asignarEmpleado'' sid cuil asignaciones) empleados) -- O(1) + O(S+E) de asignarEmpleado'' 
                        Nothing = (ConsE (asignarEmpleado'' sid cuil asignaciones) (assocM cuil (consEmpleado cuil) empleados)) -- O(log S + 2 log E + 1) 
--                                       \------------    O(log (S+E))  -------/  \-------------   O(log E)    -------------/
--  COSTO FINAL: O(log (S+E))


asignarEmpleado'' :: SectorId -> CUIL -> Map SectorId (Set Empleado)
asignarEmpleado'' sid cuil asignaciones = assocM sid addS cuil fromJust lookupM sid asignaciones
--                                        \O(log S)/    \O(E)/      \O(1)/  \O(log S)/
-- COSTO FINAL: O(log S) + O(log E) = O(log (S+E))


agregarASector sid cuil empresa = agregarEmpleado' sid cuil empresa -- COSTO FINAL: O(log (S+E))

--                                                             
borrarEmpleado cuil (ConsE asignaciones empleados) = case lookupM cuil empleados of
                        Just em = (ConsE (desasignarDeTodo cuil asignaciones) (deleteM cuil empleados)) -- O(1) + O(S+E) de asignarEmpleado'' 
                        Nothing = (ConsE asignaciones empleados)

desasignarDeTodo :: CUIL -> (Map SectorId (Set Empleado)) -> (Map SectorId (Set Empleado))
desasignarDeTodo cuil asignaciones = desasignarDeTodo' (keysM asignaciones) cuil asignaciones 

desasignarDeTodo' :: [SectorId] -> CUIL -> (Map SectorId (Set Empleado))
desasignarDeTodo' []     _    asignaciones = asignaciones 
desasignarDeTodo' (s:ss) cuil asignaciones = assocM s (lookupM s (deleteM cuil asignaciones)) (desasignarDeTodo' ss cuil asignaciones)
-- TODO: COSTOS (error: cerebro frito)