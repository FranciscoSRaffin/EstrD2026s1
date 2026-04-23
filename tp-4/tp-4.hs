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
cantCapasPorPizza []   = []
cantCapasPorPizza p:ps = ((cantidadDeCapas p), p) : cantCapasPorPizza ps

-- 2 Mapa del tesoro (Con bifurcaciones)

