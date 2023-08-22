
-- Ejercicio 2.1
--A
sucesor :: Int -> Int
sucesor a = a+1

--B
sumar :: Int -> Int -> Int
sumar a b = a + b 

--C
divisionYResto :: Int -> Int -> (Int, Int)
-- Precondicion: b es distinto de 0 
divisionYResto a b = (div a b , mod a b)

--D 
maxDelPar :: (Int, Int) -> Int
maxDelPar (a, b) = if a > b 
                   then a 
                   else b 

-- Ejercicio 2.2
{- Ejemplos de expresiones que denoten el numero 10, utilizando en cada expresion todas las funciones del punto anterior 
    1- sumar (maxDelPar (divisionYResto 10 2)) (sucesor 4)
    2- sucesor (sumar (maxDelPar (divisionYResto 10 3)) 6)
    3- maxDelPar (divisionYResto (sumar 50 50) (sucesor 9))
    4- sumar (maxDelPar (divisionYResto 100 20)) (sucesor 4)
-}

-- Ejercicio 3.1

data Dir = Norte | Sur | Este | Oeste
    deriving Show

--A 
-- Realizado con PM. Aplicando a cada Dir un caso de su opuesto.
opuesto :: Dir -> Dir 
opuesto Norte = Sur 
opuesto Este = Oeste
opuesto Sur = Norte 
opuesto Oeste = Este 

--B 
-- Realizado con PM. Aplicando a cada combinacion de pares de Dir un caso posible.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte  = True
iguales Este Este    = True
iguales Sur Sur      = True 
iguales Oeste Oeste  = True 
iguales _ _ = False

--C 
siguiente :: Dir -> Dir 
-- Precondiciones: a es distinto a Oeste
siguiente a = 
    case a of 
    Norte -> Este 
    Este -> Sur
    Sur -> Oeste 
    Oeste -> error "Oeste no tiene direccion siguiente."

-- Ejercicio 3.2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 
    deriving Show

primerDiaDeSemana = Lunes
ultimoDiaDeSemana = Domingo

--A 
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDiaDeSemana, ultimoDiaDeSemana)

--B 
empiezaConM :: DiaDeSemana -> Bool 
empiezaConM Martes = True 
empiezaConM Miercoles = True 
empiezaConM _ = False

--C 
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues a b =  numeroDia a > numeroDia b 

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes     = 1
numeroDia Martes    = 2
numeroDia Miercoles = 3
numeroDia Jueves    = 4
numeroDia Viernes   = 5
numeroDia Sabado    = 6
numeroDia Domingo   = 7
--D 
-- Resuelto mediante PM. Aplicando los unicos dos casos de FALSO al principio, y el resto de casos son verdaderos
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes    = False 
estaEnElMedio Domingo  = False 
estaEnElMedio _        = True 

-- Ejercicio 3

--A 
negar :: Bool -> Bool 
negar True = False 
negar _ = True 

--B 
implica :: Bool -> Bool -> Bool 
implica False _    = True 
implica _   b   = b 

--C 
yTambien :: Bool -> Bool -> Bool 
yTambien  False _  = False
yTambien  _     b  = b        -- En caso de venir False, devuelve False, en caso de venir True, devuelve True. 

--D 
oBien :: Bool -> Bool -> Bool 
oBien True _ = True 
oBien _    b = b -- En caso de venir True: False + True (b) = True (b). En caso de venir False: False + False (b) = False (b)



-- Ejercicio 4 

data Persona = P String Int 
    deriving Show 
            --   Nombre  Edad 

-- Ejemplo de persona: 
rodrigo = P "Rodrigo" 19 
martina = P "Martina" 19 

nombre :: Persona -> String 
nombre (P n e) = n 

edad :: Persona -> Int 
edad (P n e) = e 

crecer :: Persona -> Persona 
crecer (P n e) = P n (e + 1)

cambioDeNombre :: String -> Persona -> Persona 
cambioDeNombre nn (P n e) = P nn e

esMayorQueLaOtra :: Persona -> Persona -> Bool 
esMayorQueLaOtra p1 p2 = edad p1 > edad p2 

laQueEsMayor :: Persona -> Persona -> Persona 
-- OBSERVACIONES: En caso de que p1 y p2 tengan la misma edad, devuelve p2 
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 
                     then p1 
                    else if esMayorQueLaOtra p2 p1 
                    then p2 
                    else error "Tienen la misma edad"

-- Ejercicio 4.1 

data TipoDePokemon = Agua | Fuego | Planta 
    deriving Show 
data Pokemon = Poke TipoDePokemon Int 
    deriving Show
--                                Porcentaje de energia
data Entrenador = Ent String Pokemon Pokemon 
    deriving Show
--                    Nombre 

lina   = Poke Fuego 35
polito = Poke Agua 60
pantro = Poke Planta 10
fox    = Poke Fuego 20
rodri  = Ent "Lolito" pantro polito
pablo  = Ent "Pablito" lina fox 

tipo :: Pokemon -> TipoDePokemon 
tipo (Poke t e) = t 

--A 
-- Resolucion con PM 
superaA :: Pokemon -> Pokemon -> Bool 
superaA (Poke Agua _ ) (Poke Fuego _)   = True
superaA (Poke Fuego _ ) (Poke Planta _) = True
superaA (Poke Planta _ ) (Poke Agua _)  = True 
superaA _ _                             = False  

--B 
-- Division en subtareas. Hay que convertir dos booleanos a una suma de enteros. unoSiCeroSiNo realiza esto. El bool tiene que venir de la pregunta esDeTipo x el pokemon y. esDeTipo resuelve esto 

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (Ent _ p1 p2) = unoSiCeroSiNo (esDeTipo t p1) + unoSiCeroSiNo (esDeTipo t p2) 

unoSiCeroSiNo :: Bool -> Int 
unoSiCeroSiNo True  = 1
unoSiCeroSiNo False = 0

esDeTipo :: TipoDePokemon -> Pokemon -> Bool 
esDeTipo t p = igualesTipo t (tipo p)

igualesTipo :: TipoDePokemon -> TipoDePokemon -> Bool 
igualesTipo Agua Agua      = True 
igualesTipo Planta Planta  = True 
igualesTipo Fuego  Fuego   = True
igualesTipo _      _       = False

--C 
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = listaDePokemones e1 ++ listaDePokemones e2 

listaDePokemones :: Entrenador -> [Pokemon]
listaDePokemones (Ent _ p1 p2) = p1:p2:[]


-- Ejercicio 5 
--A 
loMismo :: a -> a
loMismo a = a 

--B 
siempreSiete :: a -> Int 
siempreSiete a = 7

--C 
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- Ejercicio 6 

estaVacia :: [a] -> Bool 
estaVacia [] = True 
estaVacia _ = False

elPrimero :: [a] -> a 
--Precondiciones: La lista no es vacia. 
elPrimero (a:_) = a

sinElPrimero :: [a] -> [a]
sinElPrimero (_: xs) = xs 

splitHead :: [a] -> (a, [a])
splitHead xs = (elPrimero xs, sinElPrimero xs)