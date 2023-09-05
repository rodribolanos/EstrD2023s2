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
-- PRECONDICION: Existe unicamente un tesoro en el mapa   
caminoAlCualIr mi md = if hayTesoro mi 
                       then mi 
                       else md

