-- RECURSION SOBRE LISTAS
-- 1.a
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- 1.b
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 1.c
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x+1) : sucesores xs

-- 1.d
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

-- 1.e
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

-- 1.f
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

-- 1.g
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

-- 1.h
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones x (y:ys) = if x == y then 1 + apariciones x ys else apariciones x ys

-- 1.i
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA k (x:xs) = if x < k then x : losMenoresA k xs else losMenoresA k xs

-- 1.j
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA k (x:xs) = if length x > k then x : lasDeLongitudMayorA k xs else lasDeLongitudMayorA k xs

-- 1.k 
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []   x = x:[]  
agregarAlFinal (a:as) x = a:(agregarAlFinal as x)

-- 1.m
agregar :: [a] -> [a] -> [a]
agregar [] xs = xs
agregar (a:as) xs =  a:(agregar as xs)

-- 1.n
reversa :: [a] -> [a]
reversa [] = []
reversa (a:as) = agregarAlFinal (reversa as) a

-- 1.l
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []     _    = []
zipMaximos (a:as) []   = []
zipMaximos (a:as) (x:xs) = (max a x):(zipMaximos as xs)

-- 1.o
elMinimo :: Ord a => [a] -> a
elMinimo (x:[])   = x
elMinimo (x:xs) = if ((head xs) > x) then elMinimo (tail xs)
                                     else elMinimo xs

-- RECURSION SOBRE NUMEROS
-- 2.a
factorial :: Int -> Int
factorial x = if (x > 0) then x * (factorial x-1) else factorial x 

-- 2.b
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva x = if (x > 0) 
                    then x : (cuentaRegresiva (x-1))
                    else x:[] 

-- 2.c
repetir :: Int -> a -> [a]
repetir x y = if (x > 1) then y:(repetir (x-1) y) else y:[]

-- 2.d
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ []     = []
losPrimeros x (a:as) = if (x == 0) then a : as
                     else if (x /= 1) then a : (losPrimeros (x-1) as)
                                      else a : []

-- 2.e
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ []     = [] 
sinLosPrimeros x (a:as) = if (x == 0) then a : as
                                    else sinLosPrimeros (x-1) as


-- REGISTROS
-- 1
data Persona = P String Int
    deriving Show 


edad :: Persona -> Int
edad (P _ e) = e

sumatoriaDeEdades :: [Persona] -> Int 
sumatoriaDeEdades []   = 0
sumatoriaDeEdades (p:ps) = (edad p) + (sumatoriaDeEdades ps)

cantidadDeElementos :: [a] -> Int
cantidadDeElementos []   = 0
cantidadDeElementos (a:as) = 1 + cantidadDeElementos as

-- 1.a
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _  []   = []
mayoresA x  (p:ps) = if ((edad p) > x)
                    then p:(mayoresA x ps)
                    else mayoresA x ps

-- 1.b
promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sumatoriaDeEdades ps) (cantidadDeElementos ps)

-- 1.c
--- Recursion con 3 casos....
elMasViejo :: [Persona] -> Persona
elMasViejo []     = error "la lista no puede ser vacia"  
elMasViejo (a:[])   = a
elMasViejo (p:ps) = if ((edad p) > edad (elMasViejo ps))
                   then p
                   else elMasViejo ps


-- 2
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

----- AUXILIARES -----

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Agua Agua = True
sonMismoTipo Fuego Fuego = True
sonMismoTipo Planta Planta = True
sonMismoTipo _ _ = False

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t _) = t

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _    = 0

pokemones :: Entrenador -> [Pokemon]
pokemones (ConsEntrenador _ ps) = ps

leGanaA :: Pokemon -> Pokemon -> Bool
leGanaA (ConsPokemon Agua _)   (ConsPokemon Fuego _)  = True
leGanaA (ConsPokemon Fuego _)  (ConsPokemon Planta _) = True
leGanaA (ConsPokemon Planta _) (ConsPokemon Agua _)   = True
leGanaA _            _            = False

-- B
cantPokemonesDeEn :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonesDeEn _ []     = 0
cantPokemonesDeEn t (p:ps) = (unoSiCeroSino (sonMismoTipo (tipo p) t)) + (cantPokemonesDeEn t ps)

-- c
pokemonesDeTipo :: [Pokemon] -> TipoDePokemon -> [Pokemon]
pokemonesDeTipo []   _   = []
pokemonesDeTipo (p:ps) t = if (sonMismoTipo (tipo p) t)
                           then p:(pokemonesDeTipo ps t)
                           else (pokemonesDeTipo ps t)

cantidadQueLeGanaATodos :: [Pokemon] -> [Pokemon] -> Int
cantidadQueLeGanaATodos [] _    = 0
cantidadQueLeGanaATodos _ []    = 1
cantidadQueLeGanaATodos (p:ps) os = (unoSiCeroSino (leGanaATodos p os)) + cantidadQueLeGanaATodos ps os

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos _ []   = True 
leGanaATodos p (o:os) = (leGanaA p o) && (leGanaATodos p os)

-- d
todosLosTipos :: [TipoDePokemon]
todosLosTipos = [Agua, Fuego, Planta]

contenidoEn :: [TipoDePokemon] -> [TipoDePokemon] -> Bool
contenidoEn []     _   = True
contenidoEn (p1:ps1) ps2 = (seEncuentraDentroDe p1 ps1) && (contenidoEn ps1 ps2)

seEncuentraDentroDe :: TipoDePokemon -> [TipoDePokemon] -> Bool
seEncuentraDentroDe _ []       = False
seEncuentraDentroDe t (t1:ts1) = (sonMismoTipo t t1) || (seEncuentraDentroDe t ts1)

contieneUnPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
contieneUnPokemonDeTipo _ []     = False
contieneUnPokemonDeTipo t (p:ps) = (sonMismoTipo t (tipo p)) || (contieneUnPokemonDeTipo t ps)

----- FIN DE AUXILIARES -----


-- 2.a
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = cantidadDeElementos ps

-- 2.b
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = cantPokemonesDeEn t ps

-- 2.c
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ ps) e2 = cantidadQueLeGanaATodos (pokemonesDeTipo ps t) (pokemones e2) 

-- 2.d
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = contieneColeccion todosLosTipos ps

contieneColeccion :: [TipoDePokemon] -> [Pokemon] -> Bool
contieneColeccion []     _  = True
contieneColeccion (t:ts) ps = (contieneUnPokemonDeTipo t ps) && (contieneColeccion ts ps)

-- 3
data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

-- 3.1 
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectosRoles rs

proyectosRoles :: [Rol] -> [Proyecto]
proyectosRoles []     = []
proyectosRoles (r:rs) = (proyecto r) : (proyectosRoles rs)

proyecto :: Rol -> Proyecto
proyecto (Management _ p) = p
proyecto (Developer _ p) = p

roles :: Empresa -> [Rol]
roles (ConsEmpresa rs) = rs

-- 3.2
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = longitud (asignadosAAlgunProyecto (seniorDevs rs) ps)

asignadosAAlgunProyecto :: [Rol] -> [Proyecto] -> [Rol]
asignadosAAlgunProyecto []     _  = []
asignadosAAlgunProyecto _      [] = []
asignadosAAlgunProyecto (r:rs) ps = if (perteneceProyecto (proyecto r) ps)
                                    then r:(asignadosAAlgunProyecto rs ps)
                                    else asignadosAAlgunProyecto rs ps

perteneceProyecto :: Proyecto -> [Proyecto] -> Bool 
perteneceProyecto _  []   = False
perteneceProyecto p1 (p:ps) = ((nombreProyecto p1) == (nombreProyecto  p)) || (perteneceProyecto p1 ps)

seniorDevs :: [Rol] -> [Rol]
seniorDevs [] = [] 
seniorDevs (r:rs) = if ((esDev r) && (esRolSr r))
                  then r:(seniorDevs rs)
                  else seniorDevs rs

nombreProyecto :: Proyecto -> String 
nombreProyecto (ConsProyecto n) = n

esDev :: Rol -> Bool
esDev (Developer _ _) = True
esDev _               = False

esRolSr ::  Rol -> Bool
esRolSr (Developer  s _) = esSenioritySr s 
esRolSr (Management s _) = esSenioritySr s

esSenioritySr :: Seniority -> Bool
esSenioritySr Senior = True
esSenioritySr _      = False

-- 3.3
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn []     _ = 0
cantQueTrabajanEn (p:ps) e = ((rolesAsignados (roles e) p)) + (cantQueTrabajanEn ps e)

rolesAsignados :: [Rol] -> Proyecto -> Int
rolesAsignados []     _ = 0
rolesAsignados (r:rs) p  = unoSiCeroSino (estaAsignado r p) + rolesAsignados rs p

estaAsignado :: Rol -> Proyecto -> Bool
estaAsignado r p = (nombreProyecto (proyecto r)) == (nombreProyecto p)
