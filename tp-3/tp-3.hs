data Color = Azul | Rojo 
    deriving Show
data Celda = CeldaVacia | Bolita Color Celda 
    deriving Show
-- Ejemplo = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda2 = CeldaVacia 
celda3 = Bolita Azul CeldaVacia 

nroBolitas :: Color -> Celda -> Int 
nroBolitas _      CeldaVacia      = 0
nroBolitas color (Bolita c celda) = unoSi (esDeColor color c) + nroBolitas color celda 

esDeColor :: Color -> Color -> Bool
esDeColor Azul Azul = True 
esDeColor _    Rojo = True 
esDeColor _    _    = False 

unoSi :: Bool -> Int 
unoSi True = 1 
unoSi _    = 0 

poner :: Color -> Celda -> Celda 
poner color celda = Bolita color celda 

sacar :: Color -> Celda -> Celda 
sacar color CeldaVacia       = CeldaVacia
sacar color (Bolita c celda) = if (esDeColor color c)
                               then sacar color celda  
                               else (Bolita c (sacar color celda) ) 

ponerN :: Int -> Color -> Celda -> Celda
--PRECONDICION: i >= 0 
ponerN 0 color c = c 
ponerN n color c = Bolita color (ponerN (n-1) color c)

data Objeto = Cachorro | Tesoro 
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
-- Ejemplo de Camino 

camino1 = Cofre [Tesoro, Cachorro, Tesoro] (Nada (Cofre [Cachorro] Fin))

hayTesoro :: Camino -> Bool 
hayTesoro Fin           = False
hayTesoro (Nada c)      = hayTesoro c
hayTesoro (Cofre obs c) = existeTesoro obs || hayTesoro c 

existeTesoro :: [Objeto] -> Bool 
existeTesoro []     = False 
existeTesoro (o:os) = esTesoro o || existeTesoro os

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True 
esTesoro _      = False 

pasosHastaTesoro :: Camino -> Int
--PRECONDICION: Tiene que haber al menos un tesoro en el camino. 
pasosHastaTesoro Fin          = 0
pasosHastaTesoro (Nada c)     = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre os c) = if (existeTesoro os)
                                then 0 
                                else 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
-- Indica true si hay un tesoro en la cantidad exacta de pasos
--PRECONDICION: i >= 0  
hayTesoroEn 0 c            = hayUnTesoroAca c 
hayTesoroEn n Fin          = False   
hayTesoroEn n (Nada c)     = hayTesoroEn (n-1) c 
hayTesoroEn n (Cofre os c) = hayTesoroEn (n-1) c         

hayUnTesoroAca :: Camino -> Bool 
hayUnTesoroAca (Cofre os _) = existeTesoro os
hayUnTesoroAca _            = False

hayAlMenosNTesoros :: Int -> Camino -> Bool 
hayAlMenosNTesoros 0 c            = True 
hayAlMenosNTesoros n (Fin)        = False 
hayAlMenosNTesoros n (Nada c)     = hayAlMenosNTesoros n c 
hayAlMenosNTesoros n (Cofre os c) = if n >= cantTesoros os  
                                    then hayAlMenosNTesoros (n - cantTesoros os) c
                                    else hayAlMenosNTesoros 0 c     

tesorosEn :: Camino -> Int 
tesorosEn Fin          = 0
tesorosEn (Nada c)     = tesorosEn c 
tesorosEn (Cofre os c) = cantTesoros os + tesorosEn c

cantTesoros :: [Objeto] -> Int 
cantTesoros []     = 0 
cantTesoros (o:os) = unoSi (esTesoro o) + cantTesoros os

cantTesorosEntre :: Int -> Int -> Camino -> Int
--PRECONDICION: i >= 0. j >= i
-- Dado un rango de pasos indica la cantidad de tesoros que hay en ese rango 
cantTesorosEntre 0 m c            = cantTesorosEntrePasos m c 
cantTesorosEntre n m Fin          = 0
cantTesorosEntre n m (Nada c)     = cantTesorosEntre (n-1) (m-1) c 
cantTesorosEntre n m (Cofre os c) = cantTesorosEntre (n-1) (m-1) c

cantTesorosEntrePasos :: Int -> Camino -> Int
--PRECONDICION: i >= 0 
cantTesorosEntrePasos 0 c            = tesorosAca c 
cantTesorosEntrePasos n Fin          = 0
cantTesorosEntrePasos n (Nada c)     = cantTesorosEntrePasos (n-1) c
cantTesorosEntrePasos n (Cofre os c) = cantTesoros os + cantTesorosEntrePasos (n-1) c

tesorosAca :: Camino -> Int 
tesorosAca (Cofre os c) = cantTesoros os
tesorosAca _            = 0


-- ARBOLES BINARIOS 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
    deriving Show 

arbolInt :: Tree Int
arbolInt = NodeT 10 
                  (NodeT 20 
                    (NodeT 10 EmptyT EmptyT) 
                    (EmptyT))
                  (NodeT 30 EmptyT EmptyT)

arbolBool :: Tree Bool
arbolBool = NodeT True 
                  (NodeT False 
                    (NodeT True EmptyT EmptyT) 
                    (EmptyT))
                  (NodeT False EmptyT EmptyT) 
sumarT :: Tree Int -> Int 
-- Dado un arbol de enteros devuelve la suma. 
sumarT EmptyT          = 0  
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int 
-- Dado un arbol binario devuelve su cantidad de elementos. 
sizeT EmptyT          = 0        
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int 
mapDobleT EmptyT          = EmptyT 
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2) 
 
perteneceT :: Eq a => a -> Tree a -> Bool 
perteneceT _ EmptyT          = False 
perteneceT m (NodeT k t1 t2) = m == k || perteneceT m t1 || perteneceT m t2

aparacionesT :: Eq a => a -> Tree a -> Int 
aparacionesT _ EmptyT          = 0
aparicionesT k (NodeT m t1 t2) = unoSi(k==m) + aparacionesT m t1 + aparicionesT m t2 

leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT m t1 t2)         = if esEmptyT t1 && esEmptyT t2 
                                 then [m]
                                 else leaves t1 ++ leaves t2
esEmptyT :: Tree a -> Bool 
esEmptyT EmptyT = True 
esEmptyT _      = False 

height :: Tree a -> Int 
-- Dado un arbol binario devuelve su altura. Cantidad de niveles maximo.
height EmptyT          = 0
height (NodeT m t1 t2) = 1 + max (height t1) (height t2)

mirrorT :: Tree a -> Tree a 
-- Dado un arbol devuelve el arbol resultante de intercambiar el hijo izq por el derecho 
mirrorT EmptyT          = EmptyT 
mirrorT (NodeT k t1 t2) = NodeT k t2 t1

toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT k t1 t2) = k : toList t2 ++ toList t1

levelN :: Int -> Tree a -> [a]
--PRECONDICION: i >= 0
levelN 0 (NodeT k t1 t2) = [k]
levelN n EmptyT          = []
levelN n (NodeT k t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

listPerLevel :: Tree a -> [[a]]
-- Dado un arbol devuelve una lista de listas en la que cada elementeo rep un nivel del arbol
listPerLevel EmptyT          = []
listPerLevel (NodeT k t1 t2) = [k] : listaPorNiveles (listPerLevel t1) (listPerLevel t2)      

listaPorNiveles :: [[a]] -> [[a]] -> [[a]]
listaPorNiveles [] xss = xss 
listaPorNiveles zss [] = zss 
listaPorNiveles (zs:zss) (xs:xss) = (zs ++ xs) : listaPorNiveles zss xss       

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT k t1 t2) = k : listaMasLarga (ramaMasLarga t1) (ramaMasLarga t2)

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga a b = if longitud a > longitud b 
                    then a 
                    else b 

longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT         = []
todosLosCaminos (NodeT k t1 t2) = [k] : (agregarElemento k (todosLosCaminos t1)) ++ (agregarElemento k (todosLosCaminos t2) )
 
agregarElemento :: a -> [[a]] -> [[a]]
-- Agrega el elemento dado a cada lista de la lista de listas. 
agregarElemento _ []       = [] 
agregarElemento k (xs:xss) = (k:xs) : (agregarElemento k xss)


-- Expresiones aritmeticas 

data ExpA = Valor Int |
            Sum ExpA ExpA |
            Prod ExpA ExpA |
            Neg ExpA
    deriving Show 

ejemploA = Sum (Valor 25) (Valor 10)
ejemploB = Prod (Sum (Valor 25) (Valor 10)) (Valor 2)
ejemploC = Neg (Prod (Sum (Valor 25) (Valor 10)) (Valor 2))

eval :: ExpA -> Int 
eval (Valor n)    = n 
eval (Sum e1 e2)  = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg e1)     = -(eval e1) 


 
{- Simplifica segun los siguientes criterios:
    a) 0 + x = x + 0 = x     
    b) 0 * x = x * 0 = 0 
    c) 1 * x = x * 1 = x 
    d) - (-x) = x         -}
simplificar :: ExpA -> ExpA
simplificar (Valor n) = Valor n 
simplificar (Sum e1 e2) = simplificarSuma (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2) 
simplificar (Neg e1)    = simplificarNeg (simplificar e1) 

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma e1        (Valor 0) = e1 
simplificarSuma (Valor 0) e2        = e2 
simplificarSuma e1        e2        = Sum e1 e2
 
simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) e2        = Valor 0 
simplificarProd e1        (Valor 0) = Valor 0
simplificarProd (Valor 1) e2        = e2 
simplificarProd e1        (Valor 1) = e1 
simplificarProd e1        e2        = Prod e1 e2

simplificarNeg :: ExpA -> ExpA 
simplificarNeg (Neg e1)) = e1  
simplificarNeg e1        = (Neg e1)  
