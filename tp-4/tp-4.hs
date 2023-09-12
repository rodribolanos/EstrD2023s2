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
propulsionEnMotoresEn (c:cs) = propulsionEnComponente c + propulsionEnMotoresEn cs

propulsionEnComponente :: Componente -> Int  
propulsionEnComponente (Motor i) = i
propulsionEnComponente _         = 0


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


-- EJERCICIO 3.5 --------------------
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave 
-- Incorpora un tripulante a una lista de sectores de la nave 
-- PRECONDICION: Existen todos los sectores de la nave 
asignarTripulanteA t sis (N s) = N (asignarTripulanteACada t sis s)

agregarTripulante :: Tripulante -> [Tripulante] -> [Tripulante] 
agregarTripulante t ts = t : ts

--Otra solucion: Con recursion sobre la lista de sectores
asignarTripulanteACada :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector 
asignarTripulanteACada t []     ts                    = ts 
asignarTripulanteACada  t (s:ss) EmptyT               = error"La lista de sectores contenia un sector no perteneciente a esta nave"
asignarTripulanteACada t (s:ss) (NodeT sector si sd)  = NodeT (agregarTripulanteSiPertence t s sector) (asignarTripulanteACada t ss si) (asignarTripulanteACada t ss sd)  

agregarTripulanteSiPertence :: Tripulante -> SectorId -> Sector -> Sector 
agregarTripulanteSiPertence t sectorid (S sid cs ts) = if sectorid == sid 
                                                 then (S sid cs (agregarTripulante t ts))
                                                 else (S sid cs ts)

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
tripulantes (N ts) = sinRepetidosTripulantes (tripulantesEn ts) 

tripulantesEn :: Tree Sector -> [Tripulante]
tripulantesEn EmptyT                 = []
tripulantesEn (NodeT sector si sd)   = (tripulantesDe sector)  ++ tripulantesEn si ++ tripulantesEn sd

sinRepetidosTripulantes :: [Tripulante] -> [Tripulante]
sinRepetidosTripulantes []     = []
sinRepetidosTripulantes (t:ts) = if pertenece t ts 
                                 then  sinRepetidosTripulantes ts 
                                 else t : sinRepetidosTripulantes ts 

tripulantesDe :: Sector -> [Tripulante] 
tripulantesDe (S sid cs ts) = ts

pertenece :: Eq a => a -> [a] -> Bool 
pertenece _ []     = False 
pertenece e (x:xs) = (e == x) || pertenece e xs


-- EJERCICIO LOBOS -------------------------------------------------------

type Presa = String 
type Territorio = String 
type Nombre = String 

data Lobo   = Cria       Nombre 
            | Explorador Nombre [Territorio] Lobo Lobo 
            | Cazador    Nombre [Presa]      Lobo Lobo Lobo

data Manada = M Lobo 
loboPantro  = Cria "Pantro"
loboPolo    = Cria "Polito" 
loboLina    = Cria "Linita"
loboFox     = Cria "Fox"
loboCazador = Cazador "Cazador" ["Vaca", "Zorrino", "Caballo", "Cocodrilo", "Pato", "Serpiente"] loboLina loboFox loboPantro 
loboExplorador1 = Explorador "Explorador1" ["Rio", "Bosque", "Misiones"] loboPolo loboPantro

manadaDeLobos = M (Explorador "Explorador2" ["unqui", "Rio de Quilmes"] loboCazador loboExplorador1)
{- Otra manera de escribirlo  
    manadaDeLobos = Explorador "Explorador2" ["unqui", "Rio de Quilmes"] (Cazador "Cazador" ["Vaca", "Zorrino", "Caballo"] (Cria "Pantro") (Cria "Lina") (Cria "Fox")) 
                                                                         (Explorador "Explorador1" ["Rio", "Bosque", "Misiones"] (Cria "Polo") (Cria "Pantro"))  -}
-- EJERCICIO 4.2------------------------
buenaCaza :: Manada -> Bool 
buenaCaza (M l) = cantidadDeAlimento l > cantidadDeCrias l 

cantidadDeCrias :: Lobo -> Int 
cantidadDeCrias (Cria n)               = 1
cantidadDeCrias (Explorador _ _ l1 l2) = cantidadDeCrias l1 + cantidadDeCrias l2
cantidadDeCrias (Cazador _ _ l1 l2 l3) = cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3

cantidadDeAlimento :: Lobo -> Int
cantidadDeAlimento (Cria n)                = 0
cantidadDeAlimento (Explorador n ts l1 l2) = cantidadDeAlimento l1 + cantidadDeAlimento l2  
cantidadDeAlimento (Cazador n ps l1 l2 l3) = longitud ps + cantidadDeAlimento l1 + cantidadDeAlimento l2 + cantidadDeAlimento l3

-- EJERCICIO 4.3------------------------
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaLobo l 

elAlfaLobo :: Lobo -> (Nombre, Int) 
elAlfaLobo (Cria n)                = (n,0)
elAlfaLobo (Explorador n ts l1 l2) = elDeMasPresas (elAlfaLobo l1) (elAlfaLobo l2)
elAlfaLobo (Cazador n ps l1 l2 l3) = elDeMasPresas (n, longitud ps) (elDeMasPresas (elAlfaLobo l1) 
                                     (elDeMasPresas (elAlfaLobo l2) (elAlfaLobo l3)) )

elDeMasPresas :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elDeMasPresas (n1,i1) (n2,i2) = if i1 > i2 
                                then (n1,i1)
                                else (n2,i2)
-- EJERCICIO 4.4------------------------
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronLobo t l 

losQueExploraronLobo :: Territorio -> Lobo -> [Nombre]
losQueExploraronLobo t (Cria n)                = []
losQueExploraronLobo t (Explorador n ts l1 l2) = if exploroTerritorio t ts  
                                                 then n : (losQueExploraronLobo t l1) ++ losQueExploraronLobo t l2
                                                 else losQueExploraronLobo t l1 ++ losQueExploraronLobo t l2
losQueExploraronLobo t (Cazador n ps l1 l2 l3) = losQueExploraronLobo t l1 ++ losQueExploraronLobo t l2 ++ losQueExploraronLobo t l3 

exploroTerritorio :: Territorio -> [Territorio] -> Bool 
exploroTerritorio t ts = pertenece t ts 

-- EJERCICIO 4.5 ----------------------------------
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M tl) = exploradoresPorTerritorioLobos tl 

exploradoresPorTerritorioLobos :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioLobos (Cria n)                = []
exploradoresPorTerritorioLobos (Explorador n ts l1 l2) = (exploradorYTerritorios n ts (juntarTerritorios (exploradoresPorTerritorioLobos l1) ++ (exploradoresPorTerritorioLobos l2))) 
exploradoresPorTerritorioLobos (Cazador n ps l1 l2 l3) = (exploradoresPorTerritorioLobos l1) ++ (exploradoresPorTerritorioLobos l2) ++ (exploradoresPorTerritorioLobos l3)

exploradorYTerritorioTupla :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> (Territorio, [Nombre])
-- RECURSION SOBRE LA LISTA DE TUPLAS VERIFICANDO SI EL TERRITORIO DADO FORMA PARTE 
exploradorYTerritorioTupla n t     []          =  (t, [n])  
exploradorYTerritorioTupla n t ((t1,ns1): tsns) = if t == t1 
                                                  then (t1, agregarNombre n ns1)           
                                                  else exploradorYTerritorioTupla n t tsns 

exploradorYTerritorios ::  Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
-- RECURSION SOBRE LA LISTA DE TERRITORIOS 
exploradorYTerritorios n [] tsns     = [] 
exploradorYTerritorios n (t:ts) tsns = exploradorYTerritorioTupla n t tsns : exploradorYTerritorios n ts tsns

juntarTerritorios :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
-- RECURSION SOBRE LA LISTA DE TUPLAS, PARA PODER JUNTAR TERRITORIOS EN CASO DE QUE SE REPITAN 
juntarTerritorios []        = []
juntarTerritorios (tn:tns)  = juntarTerritoriosTupla tn tns ++ juntarTerritorios tns   

juntarTerritoriosTupla :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
-- RECURSION SOBRE LA LISTA DE TUPLAS
juntarTerritoriosTupla tn []          = [tn]
juntarTerritoriosTupla tn (tns:tnss)  = if fst tn == fst tns   -- ESTO INDICA QUE LOS TERRITORIOS SON IGUALES, YA QUE PIDE EL PRIMERO DE CADA TUPLA 
                                        then  ((fst tns), (snd tn ++ snd tns)) : tnss 
                                        else tns : juntarTerritoriosTupla tn tnss

agregarNombre :: Nombre -> [Nombre] -> [Nombre]
agregarNombre n ns = n : ns 

-- EJERCICIO 4.6 -------------------    
superioresDelCazador :: Nombre -> Manada -> [Nombre] 
-- dado un nombre de cazador y una manada, indica el nombre de todos los cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
-- PRECONDICION: Existe un unico cazador con el nombre dado
superioresDelCazador n (M l) = superioresDelCazadorEnLobo n l 

superioresDelCazadorEnLobo :: Nombre -> Lobo -> [Nombre]
-- PRECONDICION: Existe un unico cazador con el nombre dado 
superioresDelCazadorEnLobo n (Cria n1)                =  error"No esta el lobo"
superioresDelCazadorEnLobo n (Explorador n1 ts l1 l2) =  n1 : (superioresDelCazadorEnLobo n (caminoPorDebajoDelLobo n l1 l2))                                                       
superioresDelCazadorEnLobo n (Cazador n1 ps l1 l2 l3) =  if n == n1 
                                                         then [] 
                                                         else n1 : (superioresDelCazadorEnLobo n (caminoPorDebajoDelLobo n l1 (caminoPorDebajoDelLobo n l2 l3)))
                                                         
                                                         

caminoPorDebajoDelLobo :: Nombre -> Lobo -> Lobo -> Lobo 
caminoPorDebajoDelLobo n l1 l2 = if elLoboSeEncuentraEn n l1 
                                 then l1 
                                 else l2

elLoboSeEncuentraEn :: Nombre -> Lobo -> Bool 
elLoboSeEncuentraEn n1 (Cria n)                     = n1 == n
elLoboSeEncuentraEn n1 (Explorador n ts l1 l2) = n1 == n || elLoboSeEncuentraEn n1 l1 || elLoboSeEncuentraEn n1 l2 
elLoboSeEncuentraEn n1 (Cazador n ps l1 l2 l3) = n1 == n || elLoboSeEncuentraEn n1 l1 || elLoboSeEncuentraEn n1 l2 || elLoboSeEncuentraEn n1 l3