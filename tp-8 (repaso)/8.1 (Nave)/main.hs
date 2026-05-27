-- 1 Invariantes de nave


{- INV. REP:
    Dada la nave representada por (mapSectores sid sector) (mapAsignaciones nom tripV) (maxHeap tripH)

    - Todo tripulante (tripV), el cual se encuentra contenido en el segundo map, tiene como nombre el mismo
        nombre que se esta asignando como clave del mismo (nom) en dicho mapa.
    - Todo tripulante (tripV) contenido en el segundo map (mapAsignaciones), se encuentra a su vez en el maxHeap
    - Todo tripulante contenido en el maxHeap se encuentra contenido también en el mapAsignaciones
    - Too sector al cual esta asignado un tripulante del maxHeap, se encuentra contenido en el mapSectores
-}

