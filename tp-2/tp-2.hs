-- Practica 2 

sumatoria :: [Int] -> Int 
sumatoria []     = 0 
sumatoria (x:xs) = x + sumatoria xs 

longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = (x+1) : sucesores xs 

conjuncion :: [Bool] -> Bool 
conjuncion []     = True 
conjuncion (x:xs) = x && conjuncion xs

disyucion :: [Bool] -> Bool 
disyucion []     = False
disyucion (x:xs) = x  || disyucion xs 

aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (x:xs) = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool 
pertenece _ []     = False 
pertenece e (x:xs) = (e == x) || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0 
apariciones e (x:xs) =  unoSiCeroSiNo(e == x) + apariciones e xs 

unoSiCeroSiNo :: Bool -> Int 
unoSiCeroSiNo True = 1 
unoSiCeroSiNo _    = 0 


losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []     = []
losMenoresA n (x:xs) = if (x < n)
                       then x : losMenoresA n xs 
                       else losMenoresA n xs 

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n 
                               then x : lasDeLongitudMayorA n xs 
                               else lasDeLongitudMayorA n xs 

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] b     = [b]
agregarAlFinal (x:xs) b = x : agregarAlFinal xs b 

agregar :: [a] -> [a] -> [a]
agregar [] bs     = bs 
agregar (a:as) bs = a : agregar as bs   

zipMaximos :: [Int] -> [Int] -> [Int]
-- Devuelve una lista donde el elemento en la posicion n es el maximo entre el elemento n de la primera lista y de la segunda lista. 
zipMaximos [] _          = [] 
zipMaximos _ []          = [] 
zipMaximos (x:xs) (p:ps) = maxDelPar (x,p) : zipMaximos xs ps 
    
    
    
maxDelPar :: (Int, Int) -> Int
-- OBS: En caso de ser iguales devuelve el primero. 
maxDelPar (a, b) = if a > b 
                   then a 
                   else b 

elMinimo :: Ord a => [a] -> a
-- PRECONDICION: La lista tiene al menos 1 elemento  
elMinimo []     = error "La lista no tiene elementos" 
elMinimo [a]    = a
elMinimo (x:xs) = if x < elMinimo xs 
                  then x 
                  else elMinimo xs  



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
losPrimeros 0  _     = []
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
mayoresA _ []     = [] 
mayoresA n (x:xs) = if edad x > n 
                    then x : mayoresA n xs
                    else mayoresA n xs  

edad :: Persona -> Int 
edad (ConsP _ e) = e 

promedioEdad :: [Persona] -> Int 
promedioEdad xs = div (sumaDeEdades xs) (longitud xs)  

sumaDeEdades :: [Persona] -> Int 
sumaDeEdades []     = 0 
sumaDeEdades (x:xs) = edad x + sumaDeEdades xs 

elMasViejo :: [Persona] -> Persona
--PRECONDICION: La lista posee al menos una persona 
elMasViejo []     = error "La lista no tiene ninguna persona."
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
martu  = ConsEntrenador "Gorda" [lina, fox] 

-- A 
cantPokemon :: Entrenador -> Int 
cantPokemon (ConsEntrenador _ ps) = longitud ps 

-- B 
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantPokemonDe t (ConsEntrenador _ ps) = cantPokemonDeTipo_En t ps

cantPokemonDeTipo_En :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeTipo_En _ [] = 0
cantPokemonDeTipo_En t (p:ps) = unoSiCeroSiNo (esDeTipo t p) + cantPokemonDeTipo_En t ps 

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
cuantosDeTipo_LeGananATodosLosDe_ t e1 e2 = if tieneDeTipo t e1 && losPokemonsDe_SonDerrotadosPor e2 t 
                                            then cantPokemonDe t e1 
                                            else 0  

pokemonsDe :: Entrenador -> [Pokemon] 
pokemonsDe (ConsEntrenador _ ps) = ps

losPokemonsDe_SonDerrotadosPor :: Entrenador -> TipoDePokemon -> Bool
losPokemonsDe_SonDerrotadosPor e t = laLista_EsDerrotadaPor (pokemonsDe e) t

laLista_EsDerrotadaPor :: [Pokemon] -> TipoDePokemon -> Bool 
laLista_EsDerrotadaPor [] _ = True 
laLista_EsDerrotadaPor (x:xs) t = superaATipo t (tipo x) && laLista_EsDerrotadaPor xs t  

superaAPokemon :: Pokemon -> Pokemon -> Bool 
superaAPokemon (ConsPokemon  t1 _ )  (ConsPokemon t2 _)  = superaATipo t1 t2 

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
p3  = Developer Senior obj2 
p4  = Management Senior edd

unq = ConsEmpresa [programador1, p2, p3, p4] 
proyectos :: Empresa -> [Proyecto]
proyectos e1 = sinRepetidosProyectos (proyectosDeRoles (roles e1))

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles   []    = [] 
proyectosDeRoles (x:xs) = proyecto x : proyectosDeRoles xs 

sinRepetidosProyectos :: [Proyecto] -> [Proyecto]
sinRepetidosProyectos [] = []
sinRepetidosProyectos (x:xs) = if not (estaEnLaLista x xs) 
                               then x : sinRepetidosProyectos xs 
                               else sinRepetidosProyectos xs 

estaEnLaLista :: Proyecto -> [Proyecto] -> Bool 
estaEnLaLista _ []       = False 
estaEnLaLista p1 (p2:ps) = nombreProyecto p1 == nombreProyecto p2 || estaEnLaLista p1 ps 

-- EJERCICIO 3.2 B 
losDevSenior :: Empresa -> [Proyecto] -> Int 
losDevSenior (ConsEmpresa rs) ps = cantDevSeniorEn_QueTrabajenEn rs ps

cantDevSeniorEn_QueTrabajenEn :: [Rol] -> [Proyecto] -> Int 
cantDevSeniorEn_QueTrabajenEn _ [] = 0 
cantDevSeniorEn_QueTrabajenEn rs (p:ps) = cantDevSeniorEn_ConProyecto rs p + cantDevSeniorEn_QueTrabajenEn rs ps 

cantDevSeniorEn_ConProyecto :: [Rol] -> Proyecto -> Int 
cantDevSeniorEn_ConProyecto [] _ = 0
cantDevSeniorEn_ConProyecto (r:rs) p = unoSiCeroSiNo (esDevSenior r && trabajaEn r p) + cantDevSeniorEn_ConProyecto rs p 

esDevSenior :: Rol -> Bool 
esDevSenior r = esDeveloper r && esSenior (seniority r) 

esDeveloper :: Rol -> Bool 
esDeveloper (Developer _ _) = True 
esDeveloper _               = False 

esSeniorElRol :: Rol -> Bool 
esSeniorElRol (Developer seniority _)  = esSenior seniority 
esSeniorElRol (Management seniority _) = esSenior seniority 

esSenior :: Seniority -> Bool 
esSenior Senior = True 
esSenior _ = False 

los_QueTrabajanEn :: [Rol] -> [Proyecto] -> [Rol] 
los_QueTrabajanEn [] _= [] 
los_QueTrabajanEn (r:rs) ps = if trabajaEnAlgun r ps
                              then r : los_QueTrabajanEn rs ps 
                              else los_QueTrabajanEn rs ps

trabajaEnAlgun :: Rol -> [Proyecto] -> Bool 
trabajaEnAlgun _  []    = False
trabajaEnAlgun r (p:ps) = nombreProyecto (proyecto r) == nombreProyecto p || trabajaEnAlgun r ps 

roles :: Empresa -> [Rol]
roles (ConsEmpresa rs) = rs 

proyecto :: Rol -> Proyecto
proyecto (Developer _ p)  = p
proyecto (Management _ p) = p

nombreProyecto :: Proyecto -> String 
nombreProyecto (ConsProyecto n) = n

seniority :: Rol -> Seniority 
seniority (Developer r _) = r
seniority (Management r _) = r

cantQueTrabajaEn :: [Proyecto] -> Empresa -> Int 
cantQueTrabajaEn [] _ = 0 
cantQueTrabajaEn (p:ps) e = empleadosQueTrabajenEn p (roles e) + cantQueTrabajaEn ps e 

empleadosQueTrabajenEn :: Proyecto -> [Rol] -> Int 
empleadosQueTrabajenEn _ []     = 0 
empleadosQueTrabajenEn p (r:rs) = unoSiCeroSiNo (trabajaEn r p) + empleadosQueTrabajenEn p rs

trabajaEn :: Rol -> Proyecto -> Bool
trabajaEn r p = nombreProyecto (proyecto r) == nombreProyecto p 


-- EJERCICIO 3.2 D

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
-- Estrategia: Obtener la lista de proyectos sin repeticion. A cada proyecto, asignarle mediante un nuevo recorrido el numero de personas involucradas en una tupla. 
asignadosPorProyecto e = listaDeProyectosConCantidad (sinRepetidosProyectos (proyectosDeRoles (roles e))) e 

listaDeProyectosConCantidad :: [Proyecto] -> Empresa -> [(Proyecto, Int)]
-- En este punto ya tengo la lista de proyectos sin repetir.
listaDeProyectosConCantidad []    _  = [] 
listaDeProyectosConCantidad (x:xs) e = (x, empleadosQueTrabajenEn x (roles e)) : listaDeProyectosConCantidad xs e          