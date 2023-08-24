-- Practica 2 

sumatoria :: [Int] -> Int 
sumatoria [] = 0 
sumatoria (x:xs) = x + sumatoria xs 

-- . longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
-- de elementos que posee.
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x+1) : sucesores xs 

conjuncion :: [Bool] -> Bool 
conjuncion [] = True 
conjuncion (x:xs) = x && conjuncion xs

disyucion :: [Bool] -> Bool 
disyucion [] = False
disyucion (x:xs) = x  || disyucion xs 

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool 
pertenece _ [] = False 
pertenece e (x:xs) = (e == x) || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0 
apariciones e (x:xs) =  unoSiCeroSiNo(e == x) + apariciones e xs 

unoSiCeroSiNo :: Bool -> Int 
unoSiCeroSiNo True = 1 
unoSiCeroSiNo _ = 0 


losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = if (x < n)
                       then x : losMenoresA n xs 
                       else losMenoresA n xs 

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n 
                               then x : lasDeLongitudMayorA n xs 
                               else lasDeLongitudMayorA n xs 

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] b = [b]
agregarAlFinal (x:xs) b = x : agregarAlFinal xs b 

agregar :: [a] -> [a] -> [a]
agregar [] bs = bs 
agregar (a:as) bs = agregar as bs  

zipMaximos :: [Int] -> [Int] -> [Int]
-- Devuelve una lista donde el elemento en la posicion n es el maximo entre el elemento n de la primera lista y de la segunda lista. 
zipMaximos [] _ = [] 
zipMaximos _ [] = [] 
zipMaximos (x:xs) (p:ps) = maxDelPar (x,p) : zipMaximos xs ps 
    
    
    
maxDelPar :: (Int, Int) -> Int
-- OBS: En caso de ser iguales devuelve el primero. 
maxDelPar (a, b) = if a > b 
                   then a 
                   else b 

--elMinimo :: Ord a => [a] -> a
-- DUDA 
--elMinimo [] =   
--elMinimo (x:xs) =  



--Ejercicio 2. Recursion sobre numeros 

factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * factorial (n-1) 

cuentaRegresiva :: Int -> [Int] 
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 _  = []
repetir n x  = x : repetir (n-1) x

losPrimeros :: Int -> [a] -> [a]
losPrimeros _ []     = []
losPrimeros 0 _      = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = [] 
sinLosPrimeros 0 li  = li 
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs 

-- REGISTROS 

data Persona = ConsP String Int 
    deriving Show 
--                   nombre edad 

rodrigo   = ConsP "Rodrigo" 20 
martina   = ConsP "Martina" 19
polo  = ConsP "Polo"    4
agustin = ConsP "Agustin" 17

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = [] 
mayoresA n (x:xs) = if edad x > n 
                    then x : mayoresA n xs
                    else mayoresA n xs  

edad :: Persona -> Int 
edad (ConsP n e) = e 

promedioEdad :: [Persona] -> Int 
promedioEdad xs = div (sumaDeEdades xs) (longitud xs)  

sumaDeEdades :: [Persona] -> Int 
sumaDeEdades [] = 0 
sumaDeEdades (x:xs) = edad x + sumaDeEdades xs 

elMasViejo :: [Persona] -> Persona
--PRECONDICION: La lista posee al menos una persona 
elMasViejo (x:[]) = x 
elMasViejo (x:xs) = if edad x > edad (elMasViejo xs)
                    then x 
                    else elMasViejo xs 

-- EJERCICIO 3 

data TipoDePokemon = Agua | Fuego | Planta 
    deriving Show 
data Pokemon       =  ConsPokemon TipoDePokemon Int
    deriving Show 
data Entrenador    = ConsEntrenador String [Pokemon]
    deriving Show

lina   = ConsPokemon  Fuego 35
polito = ConsPokemon  Agua 60
pantro = ConsPokemon  Planta 10
fox    = ConsPokemon  Fuego 20
rodri  = ConsEntrenador "Lolito"  [pantro, polito]
pablo  = ConsEntrenador "Pablito" [lina, fox] 

-- A 
cantPokemon :: Entrenador -> Int 
cantPokemon (ConsEntrenador _ ps) = longitud ps 

-- B 
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantPokemonDe t (ConsEntrenador _ ps) = longitud (losDeTipo t ps)

losDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
-- Devuelve la lista resultante unicamente con los pokemon del tipo dado 
losDeTipo _ []     = [] 
losDeTipo t (x:xs) = if esDeTipo t x
                     then x : losDeTipo t xs 
                     else losDeTipo t xs 

esDeTipo :: TipoDePokemon -> Pokemon -> Bool 
esDeTipo t p = igualesTipo t (tipo p)

igualesTipo :: TipoDePokemon -> TipoDePokemon -> Bool 
igualesTipo Agua Agua      = True 
igualesTipo Planta Planta  = True 
igualesTipo Fuego  Fuego   = True
igualesTipo _      _       = False

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t _) = t

-- C 

cuantosDeTipo_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_LeGananATodosLosDe_ t e1 e2 = cuantasVictoriasTienen_FrenteA (losDeTipo t (pokemonsDe e1)) (pokemonsDe e2)

pokemonsDe :: Entrenador -> [Pokemon] 
pokemonsDe (ConsEntrenador _ ps) = ps

cuantasVictoriasTienen_FrenteA :: [Pokemon] -> [Pokemon] -> Int 
cuantasVictoriasTienen_FrenteA [] _ = 0 
cuantasVictoriasTienen_FrenteA (x:xs) ys = cuantosPokemonsLeGana x ys + cuantasVictoriasTienen_FrenteA xs ys 

cuantosPokemonsLeGana :: Pokemon -> [Pokemon] -> Int 
cuantosPokemonsLeGana _ [] = 0 
cuantosPokemonsLeGana p (x:xs) = unoSiCeroSiNo (superaAPokemon p x) + cuantosPokemonsLeGana p xs 

superaAPokemon :: Pokemon -> Pokemon -> Bool 
superaAPokemon (ConsPokemon  t1 _ )  (ConsPokemon t2 _)  = superaATipo t1 t2
superaAPokemon (ConsPokemon  t1 _ ) (ConsPokemon t2 _)   = superaATipo t1 t2
superaAPokemon (ConsPokemon  t1 _ ) (ConsPokemon t2 _)   = superaATipo t1 t2 
superaAPokemon _                           _             = False 

superaATipo :: TipoDePokemon -> TipoDePokemon -> Bool 
superaATipo Agua Fuego   = True 
superaATipo Fuego Planta = True
superaATipo Planta Agua  = True 
superaATipo _      _     = False 

-- D 

esMaestroPokemon :: Entrenador -> Bool 
esMaestroPokemon p = tieneDeTipo Agua p && tieneDeTipo Fuego p && tieneDeTipo Planta p

tieneDeTipo :: TipoDePokemon -> Entrenador -> Bool
tieneDeTipo t (ConsEntrenador _ ps) = hayAlgunoDeTipo t ps 

hayAlgunoDeTipo :: TipoDePokemon -> [Pokemon] -> Bool 
hayAlgunoDeTipo _ [] = False 
hayAlgunoDeTipo t (p:ps) = esDeTipo t p || hayAlgunoDeTipo t ps 
-- hayAlgunoDeTipo t ps = longitud (losDeTipo t ps) > 0 




-- EJERCICIO 3.3 

data Seniority = Junior | Semisenior | Senior
    deriving Show
data Proyecto = ConsProyecto String
    deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto 
    deriving Show
data Empresa = ConsEmpresa [Rol] 
    deriving Show

edd = ConsProyecto "Estructura De Datos 2023"
obj2 = ConsProyecto "Objetos 2 2023"
obj1 = ConsProyecto "Objetos 1 2023"

programador1  = Developer Junior edd 
p2 = Management Junior obj1 
p3  = Developer Semisenior obj2 
p4  = Management Senior edd

unq = ConsEmpresa [programador1, p2, p3, p4] 
proyectos :: Empresa -> [Proyecto]
proyectos e1 = proyectosDe (roles e1)

roles :: Empresa -> [Rol]
roles (ConsEmpresa rs) = rs 

proyectosDe :: [Rol] -> [Proyecto]
proyectosDe   []    = [] 
proyectosDe (x:xs) = proyecto x : proyectosDe xs 

proyecto :: Rol -> Proyecto
proyecto (Developer _ p)  = p
proyecto (Management _ p) = p

nombreProyecto :: Proyecto -> String 
nombreProyecto (ConsProyecto n) = n
