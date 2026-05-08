

type Nombre = String
data EstadoCivil =
          SinPareja
        | EnMatrimonioCon Nombre
        | EnMatrimonioConPeroHijosCon Nombre Nombre
        deriving Show
data Persona = P Nombre EstadoCivil
        deriving Show
data GenTree =
          Hijos0 Persona
        | Hijos1 Persona GenTree
        | Hijos2 Persona GenTree GenTree
        | Hijos3 Persona GenTree GenTree GenTree
        deriving Show


brutus     = P "Brutus Malfoy II" (EnMatrimonioCon "Cierce Vyndon")
pollux     = P "Pollux Black" (EnMatrimonioCon "Irma Crabbe")
abraxas    = P "Abraxas Malfoy" (EnMatrimonioCon "(no identificada)")
walburga   = P "Walburga Black" (EnMatrimonioCon "Orion Black")
alphard    = P "Alphard Black" SinPareja
cygnus     = P "Cygnus Black III" (EnMatrimonioCon "Druella Rossier")
regulus    = P "Regulus Black" SinPareja
sirius     = P "Sirius Black" SinPareja
bellatrix  = P "Bellatrix Black" (EnMatrimonioConPeroHijosCon "Rodolphus Lestrange" "Tom Malvoro Riddle")
andromeda  = P "Andromeda Black" (EnMatrimonioCon "Edward Tonks")
narcissa   = P "Narcissa Black" (EnMatrimonioCon "Lucius Malfoy")
lucius     = P "Lucius Malfoy" (EnMatrimonioCon "Narcissa Black")
nymphadora = P "Nymphadora Tonks" (EnMatrimonioCon "Remus Lupin")
delphini   = P "Delphini" SinPareja
edward     = P "Edward Remus Lupin" SinPareja
draco      = P "Draco Malfoy" (EnMatrimonioCon "Astoria Greengrass")
scorpius   = P "Scorpius Malfoy" (EnMatrimonioCon "Lilly Potter")

fliaBlack = Hijos3 pollux
              (Hijos2 walburga (Hijos0 regulus) (Hijos0 sirius))
              (Hijos0 alphard)
              (Hijos3 cygnus
                 (Hijos1 bellatrix (Hijos0 delphini))
                 (Hijos1 andromeda (Hijos1 nymphadora (Hijos0 edward)))
                 (Hijos1 narcissa (Hijos1 draco (Hijos0 scorpius))))
               
fliaMalfoy = Hijos1 brutus
               (Hijos1 abraxas
                  (Hijos1 lucius (Hijos1 draco (Hijos0 scorpius))))

-- Describe la persona que encabeza el árbol genealógico dado
cabezaDeFamiliaDe :: GenTree -> Persona 
cabezaDeFamiliaDe (Hijos0 p)       = p
cabezaDeFamiliaDe (Hijos1 p _)     = p
cabezaDeFamiliaDe (Hijos2 p _ _)   = p
cabezaDeFamiliaDe (Hijos3 p _ _ _) = p

-- Describe los hijos de la persona con nombre dado, o Nothing si ninguna persona del árbol tiene ese nombre.
hijosDe_En_ :: Nombre -> GenTree -> Maybe [Persona]
hijosDe_En_ nom (Hijos0 p) = if nombrePersona p == nom -- NOTA: Esta equidad se puede hacer ya que ambos types por atrás son Strings
                            then Just [] 
                            else Nothing
hijosDe_En_ nom (Hijos1 p gt) = if nombrePersona p == nom 
                            then Just [cabezaDeFamiliaDe gt]
                            else hijosDe_En_ nom gt
hijosDe_En_ nom (Hijos2 p gt1 gt2) = if nombrePersona p == nom 
                            then Just [cabezaDeFamiliaDe gt1, cabezaDeFamiliaDe gt2] 
                            else elegirEntre (hijosDe_En_ nom gt1) (hijosDe_En_ nom gt2)
hijosDe_En_ nom (Hijos3 p gt1 gt2 gt3) = if nombrePersona p == nom
                            then Just [cabezaDeFamiliaDe gt1, cabezaDeFamiliaDe gt2, cabezaDeFamiliaDe gt3] 
                            else elegirEntre (elegirEntre (hijosDe_En_ nom gt1) (hijosDe_En_ nom gt2)) (hijosDe_En_ nom gt3)               

nombrePersona :: Persona -> Nombre
nombrePersona (P n _) = n




-- Describe los ancestros de la persona con nombre dado, o falla con error si ninguna persona del árbol tiene ese nombre.
-- Puede suponerse (sin verificar) que los nombres de las personas son únicos en el árbol dado.

ancestrosDe_En_ :: Nombre -> GenTree -> [Persona]
ancestrosDe_En_ nom (Hijos0 p)          = if nombrePersona p == nom then [] else error "No existe persona con ese nombre en el arbol"
ancestrosDe_En_ nom (Hijos1 p t1)       = if nombrePersona p == nom
                                                            then []
                                                            else p:ancestrosDe_En_ nom t1
ancestrosDe_En_ nom (Hijos2 p t1 t2)    = if nombrePersona p == nom
                                                            then []
                                                            else p:ancestrosDe_En_ nom t1 ++ ancestrosDe_En_ nom t2
ancestrosDe_En_ nom (Hijos3 p t1 t2 t3) = if nombrePersona p == nom
                                                            then []
                                                            else p:ancestrosDe_En_ nom t1 ++ ancestrosDe_En_ nom t2 ++ ancestrosDe_En_ nom t3

elegirEntre :: Maybe a -> Maybe a -> Maybe a
elegirEntre (Just x) m = Just x
elegirEntre Nothing m = m

elegirSi :: Bool -> Maybe a -> Maybe a -> Maybe a
elegirSi True  m1 m2 = m1
elegirSi False m1 m2 = m2

agregar_A_ :: a -> Maybe [a] -> Maybe [a]
agregar_A_ y Nothing = Nothing
agregar_A_ y (Just xs) = Just (y : xs)