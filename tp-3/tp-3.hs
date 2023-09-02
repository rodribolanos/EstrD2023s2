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
ponerN 0 color c = c 
ponerN n color c = Bolita color (ponerN (n-1) color c)

data Objeto = Cachorro | Tesoro 
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
-- Ejemplo de Camino 

camino1 = Cofre [Tesoro, Cachorro, Tesoro] (Nada ())

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

