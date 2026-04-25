-- 1 Pizzas

data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True
esJamon _ = False

esSalsa :: Ingrediente -> Bool 
esSalsa Salsa = True
esSalsa _ = False

esQueso :: Ingrediente -> Bool 
esQueso Queso = True
esQueso _ = False

esAceitunas :: Ingrediente -> Bool 
esAceitunas ( Aceitunas _ ) = True
esAceitunas _ = False

-- 1.1
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza     = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

-- 1.2
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = (Capa i (armarPizza is))

-- 1.3
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza   = Prepizza 
sacarJamon (Capa i p) = if (esJamon i) then sacarJamon p
                                       else (Capa i (sacarJamon p))

-- 1.4
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = (esSalsa i || esQueso i) && tieneSoloSalsaYQueso p

-- 1.5
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = (Capa (duplicarAceitunasIngrediente i) (duplicarAceitunas p)) 

duplicarAceitunasIngrediente :: Ingrediente -> Ingrediente
duplicarAceitunasIngrediente (Aceitunas cant) = (Aceitunas (cant*2)) 
duplicarAceitunasIngrediente i                = i

-- 1.6
cantCapasPorPizza :: [Pizza]-> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = ((cantidadDeCapas p), p) : cantCapasPorPizza ps

-- 2 Mapa del tesoro (Con bifurcaciones)
data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa


esIzq :: Dir -> Bool 
esIzq Izq = True
esIzq _   = False

-- 2.1
hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre)                             = tieneTesoroCofre cofre
hayTesoro (Bifurcacion cofre caminoIzq caminoDer) = (tieneTesoroCofre cofre) || (hayTesoro caminoIzq) || (hayTesoro caminoDer)

tieneTesoroCofre :: Cofre -> Bool
tieneTesoroCofre (Cofre objs) = tieneTesoroItems objs

tieneTesoroItems :: [Objeto] -> Bool
tieneTesoroItems []     = False
tieneTesoroItems (o:os) = (esTesoro o) || tieneTesoroItems os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-- 2.2
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     (Fin cofre)         = tieneTesoroCofre cofre 
hayTesoroEn (di:ds) (Bifurcacion _ i d) = if (esIzq di) then hayTesoroEn ds i
                                                    else hayTesoroEn ds d
hayTesoroEn _    _                   = False

-- 2.3
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre)             = [] 
caminoAlTesoro (Bifurcacion cofre i d) =  if (hayTesoro i)
                                              then Izq : caminoAlTesoro i
                                              else Der : caminoAlTesoro d

-- 2.4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga m = caminoMasLargo (todosLosCaminos m)

caminoMasLargo :: [[a]] -> [a]
caminoMasLargo xss = elPrimero (ordenarPorMasLargo xss)

elPrimero :: [a] -> a
elPrimero (x:_) = x

consACada :: a -> [[a]] -> [[a]]
consACada y []       = [y:[]]
consACada y (xs:xss) = (y:xs):(consACada y xss)

ordenarPorMasLargo :: [[a]] -> [[a]]
ordenarPorMasLargo []       = []
ordenarPorMasLargo (xs:xss) = insertar xs (ordenarPorMasLargo xss)

insertar :: [a] -> [[a]] -> [[a]]
insertar xs []       = xs:[]
insertar xs (ys:yss) = if (cantidadDeElementos xs > cantidadDeElementos ys)
                        then xs:ys:yss
                        else ys:(insertar xs yss)

cantidadDeElementos :: [a] -> Int
cantidadDeElementos []   = 0
cantidadDeElementos (a:as) = 1 + cantidadDeElementos as


-- 2.5
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)             = [tesoros c]
tesorosPorNivel (Bifurcacion c i d) = [tesoros c] ++ zipNiveles (tesorosPorNivel i) (tesorosPorNivel d)

--            A
--         B    C
--       D  E  F  G
--
--   i [[b], [D,E]]
--   d [[C],[F,G]]
--      -> [[A],[B,C],[D,E,F,G]]

zipNiveles :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
zipNiveles []       yss      = yss 
zipNiveles xss      []       = xss
zipNiveles (xs:xss) (ys:yss) = (xs ++ ys) : zipNiveles xss yss

-- [A,B] ++ [C,D] -> [A,B,C,D] 
-- [A,B,C,D] :

tesoros :: Cofre -> [Objeto] 
tesoros (Cofre objetos) = filtrarTesoros objetos

filtrarTesoros :: [Objeto] -> [Objeto]
filtrarTesoros []     = []
filtrarTesoros (o:os) = (singularSi o (esTesoro o)) ++ filtrarTesoros os

singularSi :: a -> Bool -> [a]
singularSi x True  = x:[]
singularSi x False = []


-- 2.6
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)             = []
todosLosCaminos (Bifurcacion _ i d) = (consACada Izq (todosLosCaminos i)) ++ (consACada Der (todosLosCaminos d))


-- 3 Nave Espacial 

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)

sectorId :: Sector -> SectorId
sectorId (S id _ _) = id

componentesDeSector :: Sector -> [Componente]
componentesDeSector (S _ componentes _) = componentes

componentesDeSectores :: (Tree Sector) -> [Componente]
componentesDeSectores EmptyT               = [] 
componentesDeSectores (NodeT sect izq der) = componentesDeSector sect ++ componentesDeSectores izq ++ componentesDeSectores der

barrilesDeComponentes :: [Componente] -> [Barril]
barrilesDeComponentes []     = []
barrilesDeComponentes (c:cs) = (barrilesDeComponente c) ++ barrilesDeComponentes cs

barrilesDeComponente :: Componente -> [Barril]
barrilesDeComponente (Almacen barriles) = barriles
barrilesDeComponente _                  = []

-- 3.1
sectores :: Nave -> [SectorId]
sectores (N sectores) = idDeSectores sectores

idDeSectores :: (Tree Sector) -> [SectorId]
idDeSectores EmptyT               = []
idDeSectores (NodeT sect izq der) = sectorId sect : (idDeSectores izq ++ idDeSectores der)

-- 3.2
poderDePropulsion :: Nave -> Int
poderDePropulsion (N sectores) = poderDePropulsionSectores sectores

poderDePropulsionSectores :: (Tree Sector) -> Int
poderDePropulsionSectores EmptyT               = 0
poderDePropulsionSectores (NodeT sect izq der) = poderDePropulsionSector sect + poderDePropulsionSectores izq + poderDePropulsionSectores der

poderDePropulsionSector :: Sector -> Int
poderDePropulsionSector (S _ componentes _) = poderDePropulsionComponentes componentes

poderDePropulsionComponentes :: [Componente] -> Int
poderDePropulsionComponentes []     = 0
poderDePropulsionComponentes (c:cs) = poderDePropulsionComponente c + poderDePropulsionComponentes cs

poderDePropulsionComponente :: Componente -> Int
poderDePropulsionComponente (Motor poder) = poder
poderDePropulsionComponente _             = 0

-- 3.3
barriles :: Nave -> [Barril]
barriles (N sectores) = barrilesDeComponentes (componentesDeSectores sectores)

-- 3.4
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sId (N sectoresT) = (N (agregarASectorEnSectoresT cs sId sectoresT))

agregarASectorEnSectoresT :: [Componente] -> SectorId -> (Tree Sector) -> (Tree Sector)
agregarASectorEnSectoresT _  _   EmptyT               = EmptyT
agregarASectorEnSectoresT cs sId (NodeT sect izq der) = if ((sectorId sect) == sId)
                                                           then (NodeT (agregarASectorEnSector cs sect) izq der) 
                                                           else (NodeT sect (agregarASectorEnSectoresT cs sId izq)
                                                                            (agregarASectorEnSectoresT cs sId der)
                                                                )

agregarASectorEnSector :: [Componente] -> Sector -> Sector
agregarASectorEnSector cs (S id ys ts) = (S id (cs ++ ys) ts) 

-- 3.5
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t sids (N sectoresT) = (N (asignarTripulanteASectores t sids sectoresT))

asignarTripulanteASectores :: Tripulante -> [SectorId] -> (Tree Sector) -> (Tree Sector)
asignarTripulanteASectores _ _    EmptyT               = EmptyT 
asignarTripulanteASectores t sids (NodeT sect izq der) = 
    (NodeT (asignarTripulanteASectorDentroDeIds t sids sect) 
           (asignarTripulanteASectores t sids izq)
           (asignarTripulanteASectores t sids der)
    ) 

asignarTripulanteASectorDentroDeIds :: Tripulante -> [SectorId] -> Sector -> Sector
asignarTripulanteASectorDentroDeIds _ []       sect = sect
asignarTripulanteASectorDentroDeIds t (si:sis) sect = if ((sectorId sect) == si)
                                                           then asignarTripulanteASector t sect
                                                           else asignarTripulanteASectorDentroDeIds t sis sect

asignarTripulanteASector :: Tripulante -> Sector -> Sector
asignarTripulanteASector t (S id cs ts) = (S id cs (t:ts))

-- 3.6
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N sectoresT) = sectoresAsignadosEnT t sectoresT

sectoresAsignadosEnT :: Tripulante -> (Tree Sector) -> [SectorId]
sectoresAsignadosEnT _ EmptyT               = []
sectoresAsignadosEnT t (NodeT sect izq der) = (singularSi (sectorId sect) (estaAsignadoEnSector t sect)) ++ sectoresAsignadosEnT t izq ++ sectoresAsignadosEnT t der

estaAsignadoEnSector :: Tripulante -> Sector -> Bool
estaAsignadoEnSector t (S _ _ ts) = perteneceTripulante t ts

perteneceTripulante :: Tripulante -> [Tripulante] -> Bool
perteneceTripulante _    []     = False
perteneceTripulante trip (t:ts) = (trip == t) || perteneceTripulante trip ts

-- 3.7 
tripulantes :: Nave -> [Tripulante]
tripulantes (N sectoresT) = sinRepeticiones (tripulantesDeSectoresT sectoresT)

tripulantesDeSectoresT :: (Tree Sector) -> [Tripulante]
tripulantesDeSectoresT EmptyT               = []
tripulantesDeSectoresT (NodeT sect izq der) = tripulantesDeSector sect ++ tripulantesDeSectoresT izq ++ tripulantesDeSectoresT der

tripulantesDeSector :: Sector -> [Tripulante]
tripulantesDeSector (S _ _ ts) = ts

sinRepeticiones :: Eq a => [a] -> [a]
sinRepeticiones []     = []
sinRepeticiones (x:xs) = if (pertenece x xs) then sinRepeticiones xs
                                             else x:(sinRepeticiones xs)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

-- 4 Manada de lobos 

type Presa = String-- nombre de presa
type Territorio = String-- nombre de territorio
type Nombre = String-- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cría Nombre
data Manada = M Lobo