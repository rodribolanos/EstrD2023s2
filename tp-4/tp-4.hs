data Pizza = Prepizza | Capa Ingrediente Pizza 
    deriving Show 
data Ingrediente = Salsa | 
                   Queso | 
                   Jamon | 
                   Aceitunas Int 
    deriving Show 
pizzaEj = Capa Queso (Capa Jamon (Capa Queso (Capa (Aceitunas 20) (Capa Queso (Capa Salsa Prepizza)))))

cantidadDeCapas :: Pizza -> Int
-- Dada una pízza devuelve su cantidad de ingredientes 
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza 
-- Dada una lista de ingredientes devuelve una pizza 
armarPizza []     = Prepizza 
armarPizza (i:is) = Capa i (armarPizza is) 

sacarJamon :: Pizza -> Pizza 
-- Dada una pizza devuelve la misma pizza sin Jamon 
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if esJamon i 
                        then sacarJamon p 
                        else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True 
esJamon _     = False

tieneSoloSalsaYQueso :: Pizza -> Bool 
tieneSoloSalsaYQueso Prepizza   = True 
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool 
esSalsaOQueso Salsa = True 
esSalsaOQueso Queso = True 
esSalsaOQueso _     = False

duplicarAceitunas :: Pizza -> Pizza
-- Dada una pizza, devuelve la misma pizza con las aceitunas duplicadas 
duplicarAceitunas Prepizza   = Prepizza 
duplicarAceitunas (Capa i p) = if esAceituna i 
                               then Capa (aceitunasDuplicadas i) (duplicarAceitunas p) 
                               else Capa i (duplicarAceitunas p)

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas i)= True 
esAceituna _        = False 

aceitunasDuplicadas :: Ingrediente -> Ingrediente
 --Precondicion: El elemento dado es una aceituna. En caso de no serlo devuelve el propio ingrediente.   
aceitunasDuplicadas (Aceitunas i) = (Aceitunas (i*2))
aceitunasDuplicadas i             = i

cantidadDeCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantidadDeCapasPorPizza []     = []
cantidadDeCapasPorPizza (p:ps) =  (cantidadDeCapas p, p) : cantidadDeCapasPorPizza ps

-- MAPA DE TESOROS 

data Dir = Izq | Der 
    deriving Show 
data Objeto = Tesoro | Chatarra 
    deriving Show 
data Cofre = Cofre [Objeto]
    deriving Show 
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa 

mapaEj = Bifurcacion (Cofre [Chatarra]) 
                        (Bifurcacion (Cofre [Chatarra])
                            (Bifurcacion (Cofre [Tesoro]) (Fin (Cofre [])) (Fin (Cofre [])))
                            (Fin (Cofre [])))
                        (Bifurcacion (Cofre [])
                            (Bifurcacion (Cofre [Chatarra]) (Fin (Cofre [])) (Fin (Cofre [])))
                            (Fin (Cofre [])))
-- EJERCICIO 2A --------------------------------------------------------------
hayTesoro :: Mapa -> Bool 
-- Dado un mapa, devuelve si hay un tesoro en alguna parte del mapa
hayTesoro (Fin c)               = hayTesoroAca c 
hayTesoro (Bifurcacion c mi md) = hayTesoroAca c || hayTesoro mi || hayTesoro md 

hayTesoroAca :: Cofre -> Bool 
-- Dado un cofre te dice si hay un tesoro en su lista de objetos 
hayTesoroAca (Cofre os) = tieneTesoros os 

tieneTesoros :: [Objeto] -> Bool 
tieneTesoros []     = False 
tieneTesoros (o:os) = esTesoro o || tieneTesoros os

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True 
esTesoro _      = False

-- EJERCICIO 2B --------------------------------------------------------------
hayTesoroEn :: [Dir] -> Mapa -> Bool 
-- Dada una lista de direcciones indica si al final de la lista hay un tesoro. Recursion sobre la lista y el mapa
-- PRECONDICION: La lista de direcciones es un camino viable para el mapa.   
hayTesoroEn []  m                             = hayTesoroAca (cofre (m))  
hayTesoroEn _  (Fin c)                       = error"No queda mapa por recorrer en esa direccion"
hayTesoroEn (dir:dirs) (Bifurcacion c mi md) = if esDer dir 
                                               then hayTesoroEn dirs md
                                               else hayTesoroEn dirs mi    

cofre :: Mapa -> Cofre 
-- Dado un mapa, devuelve el cofre en la posicion inicial del mapa 
cofre (Fin c)              = c
cofre (Bifurcacion c _ _ ) = c

esDer :: Dir -> Bool 
esDer Der = True 
esDer _   = False

-- EJERCICIO 2C --------------------------------------------------------------
caminoAlTesoro :: Mapa -> [Dir]
-- PRECONDICION: Existe unicamente un tesoro en el mapa 
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c mi md) = if hayTesoroAca c
                                       then [] 
                                       else direccionADondeIr mi : caminoAlTesoro (caminoAlCualIr mi md) 

direccionADondeIr :: Mapa -> Dir 
-- PRECONDICION: Existe unicamente un tesoro en el mapa 
-- Dado un mapa te devuelve la direccion del primer movimiento al cual ir 
direccionADondeIr m = if (hayTesoro m) 
                      then Izq 
                      else Der 

caminoAlCualIr :: Mapa -> Mapa -> Mapa
-- Dado un mapa te devuelve el camino completo por el que hay tesoro
-- PRECONDICION: Existe unicamente un tesoro en el mapa   
caminoAlCualIr mi md = if hayTesoro mi 
                       then mi 
                       else md


-- EJERCICIO 2D --------------------------------------------------------------

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- En caso de dos caminos ser igual de largos, siempre se inclinara para caminar hacia la derecha.
caminoDeLaRamaMasLarga (Fin c)               = []
caminoDeLaRamaMasLarga (Bifurcacion c mi md) = direccionCMasProfundo mi md : caminoMasLargoEntre (caminoDeLaRamaMasLarga mi) (caminoDeLaRamaMasLarga md)

direccionCMasProfundo :: Mapa -> Mapa -> Dir
direccionCMasProfundo mi md = if (profundidad mi) > profundidad md 
                              then Izq 
                              else Der  

profundidad :: Mapa -> Int 
profundidad (Fin c)               = 0
profundidad (Bifurcacion c mi md) = 1 + max (profundidad mi)  (profundidad md) 

caminoMasLargoEntre :: [a] -> [a] -> [a]
caminoMasLargoEntre a b = if longitud a > longitud b 
                    then a 
                    else b 

longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- EJERCICIO 2E --------------------------------------------------------------

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [tesorosDe c] 
tesorosPorNivel (Bifurcacion c mi md) = tesorosDe c : juntarPorNiveles (tesorosPorNivel mi) (tesorosPorNivel md) 

juntarPorNiveles :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
juntarPorNiveles []  yss           = yss
juntarPorNiveles xss []            = xss 
juntarPorNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarPorNiveles xss yss 
 
tesorosDe :: Cofre -> [Objeto]
tesorosDe (Cofre obs) = tesorosEnObjetos obs 

tesorosEnObjetos :: [Objeto] -> [Objeto] 
tesorosEnObjetos []     = []
tesorosEnObjetos (o:os) = if esTesoro o 
                          then o : tesorosEnObjetos os 
                          else tesorosEnObjetos os 

-- EJERCICIO 2F --------------------------------------------------------------

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c)               = []
todosLosCaminos (Bifurcacion c mi md) = [Izq] : (agregarIzq (todosLosCaminos mi)) ++ [Der] :(agregarDer (todosLosCaminos md)) 

agregarIzq :: [[Dir]] -> [[Dir]]
-- Agrega la direccion izquierda a todos los caminos 
agregarIzq []       = []  
agregarIzq (ds:dss) = (Izq : ds) : agregarIzq dss

agregarDer :: [[Dir]] -> [[Dir]]
-- Agrega lsa direccion izquierda a todos los caminos 
agregarDer []       = []  
agregarDer (ds:dss) = (Der : ds) : agregarDer dss

data Componente = Lanzatorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril     = Comida | Torpedo | Oxigeno | Combustible 
    deriving Show
data Sector     = S SectorId [Componente] [Tripulante] 
    deriving Show
type SectorId   = String 
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show 
data Nave = N (Tree Sector)
    deriving Show

naveEjemplo = N sectorEjemplo 

sector1 = S "sector1" [Lanzatorpedos, (Motor 4)] ["Thiago"]
sector2 = S "sector2" [(Almacen ([Comida, Torpedo, Oxigeno]))] ["Lean"]
sector3 = S "sector3" [Lanzatorpedos, (Almacen ([Comida, Torpedo, Oxigeno]))] ["Rodri"] 
sector4 = S "sector4" [(Motor 2)] ["Nacho", "Rodri"]

sectorEjemplo :: Tree Sector 
sectorEjemplo =  NodeT sector1 
                        (NodeT sector2 
                                (NodeT sector4 
                                    EmptyT 
                                    EmptyT)
                                EmptyT)
                        (NodeT sector3
                            EmptyT
                            EmptyT) 

sectores :: Nave -> [SectorId] 
-- Devuelve todos los sectores de la nave 
sectores (N s) = sectoresEn s

sectoresEn :: Tree Sector -> [SectorId]
sectoresEn EmptyT               = []
sectoresEn (NodeT sector si sd) = sectorId sector : (sectoresEn si) ++ (sectoresEn sd)

sectorId :: Sector -> SectorId
sectorId (S si _ _) = si

poderDePropulsion :: Nave -> Int
-- Devuelve la suma de poder de propulsion de todos los motores de la nave
poderDePropulsion (N s) = poderDePropulsionEn s 

poderDePropulsionEn :: Tree Sector -> Int 
poderDePropulsionEn EmptyT               = 0
poderDePropulsionEn (NodeT sector si sd) = propulsionEnSector sector + poderDePropulsionEn si + poderDePropulsionEn sd

propulsionEnSector :: Sector -> Int 
propulsionEnSector (S _ cs _) = propulsionEnMotoresEn cs

propulsionEnMotoresEn :: [Componente] -> Int 
propulsionEnMotoresEn []     = 0
propulsionEnMotoresEn (c:cs) = propulsionEn c + propulsionEnMotoresEn cs

propulsionEn :: Componente -> Int 
propulsionEn c = if esMotor c 
                 then propulsionEnMotor c
                 else 0

esMotor :: Componente -> Bool 
esMotor (Motor i) = True 
esMotor _         = False 

propulsionEnMotor :: Componente -> Int 
-- Precondicion: El componente dado es un motor 
propulsionEnMotor (Motor i) = i


-- EJERCICIO 3.3 -------------
barriles :: Nave -> [Barril] 
barriles (N ts) = barrilesEn ts

barrilesEn :: Tree Sector -> [Barril]
barrilesEn EmptyT            = []
barrilesEn (NodeT sector si sd) = barrilesEnSector sector ++ barrilesEn si ++ barrilesEn sd

barrilesEnSector :: Sector -> [Barril]
barrilesEnSector (S _ cs _) = barrilesEnComponentes cs

barrilesEnComponentes :: [Componente] -> [Barril]
barrilesEnComponentes []     = []
barrilesEnComponentes (c:cs) = if esAlmacen c 
                               then barrilesEnAlmacen c ++ barrilesEnComponentes cs 
                               else barrilesEnComponentes cs

esAlmacen :: Componente -> Bool 
esAlmacen (Almacen bs) = True 
esAlmacen _            = False 

barrilesEnAlmacen :: Componente -> [Barril]
-- PRECONDICION: El componente dado es un almacen.
barrilesEnAlmacen (Almacen bs) = bs

-- EJERCICIO 3.4 ------------------------
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- Anade una lista de componentes al sector de la nave con el nombre dado.
-- El sector puede no formar parte de la nave, en ese caso, no agrega nada
agregarASector cs sid (N ts) = N (agregarComponentes_ASectorEn cs sid ts) 

agregarComponentes_ASectorEn :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarComponentes_ASectorEn cs sid EmptyT                = EmptyT 
agregarComponentes_ASectorEn cs sid (NodeT sector si sd)  = (NodeT (agregarComponentesEn cs sid sector) 
                                                                    (agregarComponentes_ASectorEn cs sid si) 
                                                                    (agregarComponentes_ASectorEn cs sid sd))

agregarComponentesEn :: [Componente] -> SectorId -> Sector -> Sector 
agregarComponentesEn cs sid (S sectorId cos ts) = if sid == sectorId 
                                                then (S sectorId (cos ++ cs) ts)
                                                else (S sectorId cos ts)


-- EJERCICIO 3.4 --------------------
{-asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave 
-- Incorpora un tripulante a una lista de sectores de la nave 
-- PRECONDICION: Existen todos los sectores de la nave 
asignarTripulanteA t sis (N s) = N (asignarTripulanteACada t sis s)
 Solucion sin cumplir precondicion 
asignarTripulanteACada :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteACada t sis EmptyT               = EmptyT 
asignarTripulanteACada t sis (NodeT sector si sd) = NodeT (agregarTripulanteSiPertenece t sis sector)  (asignarTripulanteACada t sis si) (asignarTripulanteACada t sis sd) 

agregarTripulanteSiPertenece :: Tripulante -> [SectorId] -> Sector -> Sector 
agregarTripulanteSiPertenece t [] sector            = sector  
agregarTripulanteSiPertenece t (s:ss) (S sid cs ts) = if s == sid 
                                                      then (S sid cs (agregarTripulante t ts))
                                                      else agregarTripulanteSiPertenece t ss (S sid cs ts)


-- Solucion con precondicion. Con recursion sobre la lista de sectorId // SIGUE SIN FUNCIONAR PARA PRECONDICION. AHORA TIRA ERROR SIEMPRE  
asignarTripulanteACada :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteACada t []     ts = ts 
asignarTripulanteACada t (s:ss) ts = asignarTripulanteASector t s ts                          

 asignarTripulanteASector :: Tripulante -> SectorId -> Tree Sector -> Tree Sector
asignarTripulanteASector t sid EmptyT               = error"El sector no pertenece a esta nave"
asignarTripulanteASector t sid (NodeT sector si sd) = NodeT (agregarTripulanteSiEs t sid sector) (asignarTripulanteASector t sid si) (asignarTripulanteASector t sid sd)
    
agregarTripulanteSiEs :: Tripulante -> SectorId -> Sector -> Sector 
agregarTripulanteSiEs t sid (S id cs ts)  = if sid == id  
                                            then (S id cs (agregarTripulante t ts))
                                            else (S id cs ts)
                             
agregarTripulante :: Tripulante -> [Tripulante] -> [Tripulante] 
agregarTripulante t ts = t : ts
 -}
-- EJERCICIO 3.6 --------------------------------------
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N ts) = sectoresAsignadosPara t ts 

sectoresAsignadosPara :: Tripulante -> Tree Sector -> [SectorId]
-- Recursion sobre la estructura de arbol. 
sectoresAsignadosPara t EmptyT               = []
sectoresAsignadosPara t (NodeT sector si sd) = if esSectorAsignadoPara t sector 
                                               then obsID (sector) : (sectoresAsignadosPara t si) ++ (sectoresAsignadosPara t sd)
                                               else (sectoresAsignadosPara t si) ++ (sectoresAsignadosPara t sd)  

obsID :: Sector -> SectorId 
obsID (S sid _ _) = sid 

esSectorAsignadoPara :: Tripulante -> Sector -> Bool 
-- Subtarea para poder abstraer la lista de tripulantes del sector dado 
esSectorAsignadoPara t (S sid cs ts) = apareceElTripulante t ts 

apareceElTripulante :: Tripulante -> [Tripulante] -> Bool 
-- Recursion sobre la lista de tripulantes 
apareceElTripulante t []         = False 
apareceElTripulante t (tri:tris) = t == tri || apareceElTripulante t tris 

tripulantes :: Nave -> [Tripulante]
-- Devuelve la lista de tripulantes de la nave sin repetidos 
tripulantes (N ts) = tripulantesEn ts 

tripulantesEn :: Tree Sector -> [Tripulante]
tripulantesEn EmptyT                 = []
tripulantesEn (NodeT sector si sd)   = tripulantesQueNoAparecen sector (tripulantesEn si ++ tripulantesEn sd) ++ tripulantesEn si ++ tripulantesEn sd 