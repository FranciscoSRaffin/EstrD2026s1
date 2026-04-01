-- NUMEROS ENTEROS
-- 1.a
sucesor :: Int -> Int
sucesor n = n+1

-- 1.b
sumar :: Int -> Int -> Int
sumar x y = x+y

-- 1.c
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

-- 1.d
maxDelPar :: (Int, Int) -> Int
maxDelPar (x, y) = if (x > y) then x
                   else            y
-- 2
{-
  sumar (sucesor (maxDelPar (divisionYResto 3 2))) 8
  sumar (maxDelPar (divisionYResto 4 2)) (maxDelPar (divisionYResto 32 4))
  sumar (maxDelPar (divisionYResto 40 10)) (sumar 3 (sucesor 2))
  maxDelPar (divisionYResto (sumar 10 90) (sucesor 9))
-}


-- TIPOS ENUMERATIVOS

-- 1
data Dir = Norte | Sur | Este | Oeste
     deriving Show

-- 1.a
opuesto :: Dir -> Dir
opuesto Norte = Sur  
opuesto Sur = Norte 
opuesto Este = Oeste 
opuesto _ = Este

-- 1.b
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False
 
-- 1.c
siguiente :: Dir -> Dir
siguiente Norte = Este  
siguiente Sur = Oeste
siguiente Este = Sur 
siguiente _ = error "No existe un direccion siguiente  Oeste"

-- La funion es parcial ya que no puede usarse con la totaliad de casos ados por el 
-- tipo de dato rciido y dee de contemplarse precauciones en su uso.
-- Dado esto, si, posee una preconicion.


-- 2
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
     deriving Show

-- 2.a
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

-- 2.b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True 
empiezaConM Miercoles = True
empiezaConM _ = False

-- 2.c 
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = (posicionEnSemana d1) > (posicionEnSemana d2)

posicionEnSemana :: DiaDeSemana -> Int
posicionEnSemana Lunes = 0
posicionEnSemana Martes = 1
posicionEnSemana Miercoles = 2
posicionEnSemana Jueves = 3
posicionEnSemana Viernes = 4
posicionEnSemana Sabado = 5
posicionEnSemana Domingo = 6

-- 2.d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False 
estaEnElMedio Domingo = False
estaEnElMedio _ = True

-- 3.a
negar :: Bool-> Bool
negar True = False
negar _ = True 

-- 3.b
implica :: Bool-> Bool-> Bool
implica True False = False
implica _ _ = True

-- 3.c
yTambien :: Bool-> Bool-> Bool
yTambien True True = True
yTambien _ _ = False

-- 3.d
oBien :: Bool-> Bool-> Bool
oBien True _ = True
oBien _ b = b



-- REGISTROS

-- 1
data Persona = Per String Int
     deriving Show

-- 1.a
nombre :: Persona -> String
nombre (Per n _) = n

-- 1.b
edad :: Persona -> Int
edad (Per _ e) = e

-- 1.c
crecer :: Persona -> Persona
crecer (Per n e) = Per n (e + 1)

-- 1.d
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (Per _ e) = Per nn e

-- 1.h
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = (edad p1) > (edad p2)

-- 1.i
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2) then p1 else p2


-- 2
data Pokemon = P TipoDePokemon Int
     deriving Show
data TipoDePokemon = Agua | Fuego | Planta
     deriving Show
data Entrenador = E String Pokemon Pokemon
     deriving Show

-- 2.a
superaA :: Pokemon -> Pokemon -> Bool
superaA (P Agua _) (P Fuego _) = True
superaA (P Fuego _) (P Planta _) = True
superaA (P Planta _) (P Agua _) = True
superaA _ _ = False

-- 2.b
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = cantidadDePokemonDe_EnPokemon t p1 + cantidadDePokemonDe_EnPokemon t p2

cantidadDePokemonDe_EnPokemon :: TipoDePokemon -> Pokemon -> Int
cantidadDePokemonDe_EnPokemon t (P tp _) = if sonMismoTipo t tp then 1 else 0

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Agua Agua = True
sonMismoTipo Fuego Fuego = True
sonMismoTipo Planta Planta = True
sonMismoTipo _ _ = False

-- 2.c
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ p1 p2), (E _ p3 p4)) = [p1, p2, p3, p4]


-- FUNCIONES POLIMORFICAS

-- 1.a
loMismo :: a -> a
loMismo a = a

-- 1.b
siempreSiete :: a -> Int
siempreSiete _ = 7

-- 1.c
swap :: (a,b) -> (b, a)
swap (a,b) = (b,a)


-- 2. Responda la siguiente pregunta: ¾Por qué estas funciones son polimórficas?
-- Porque pueden recibir datos de cualquier tipo



-- PATTERN MATCHING SOBRE LISTAS

-- 1.a [...]


-- 1.b
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

-- 1.c
elPrimero :: [a] -> a
elPrimero (x:_) = x

-- 1.d
sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs

-- 1.e
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)


