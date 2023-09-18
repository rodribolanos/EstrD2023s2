-- CALCULO DE COSTOS. 
-- EJERCICIO 1. 

import SetV2 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

head2 :: [a] -> a 
head2 (x:xs) = x
-- COSTO CONSTANTE. No importa la cantidad de datos. 

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
-- COSTO CONSTANTE.

factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * (factorial n-1)
-- COSTO LINEAL. Por cada elemento realiza una operacion de costo constante. 

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
-- COSTO LINEAL. Por cada elemento realiza una operacion de costo constante.

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
-- COSTO CUADRATICO: Realiza una operacion de costo lineal por cada elemento.

pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs
-- COSTO LINEAL. Realiza una operacion de costo constante por cada elemento de la lista. 

--sinRepetidos :: Eq a => [a] -> [a]
--sinRepetidos [] = []
--sinRepetidos (x:xs) =   if pertenece x xs
--                        then sinRepetidos xs
--                        else x : sinRepetidos xs
-- COSTO CUADRATICO. Realiza una operacion de costo lineal por cada elemento. 

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys     = ys
append (x:xs) ys = x : append xs ys
-- COSTO LINEAL. Realiza una operacion de costo constante por cada elemento.

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
-- COSTO CUADRATICO. Realiza una operacion de costo lineal por cada elemento.

takeN :: Int -> [a] -> [a]
takeN 0 xs     = []
takeN n []     = []
takeN n (x:xs) = x : takeN (n-1) xs
-- COSTO LINEAL. Por cada elemento realiza una operacion de costo lineal.

dropN :: Int -> [a] -> [a]
dropN 0 xs     = xs
dropN n []     = []
dropN n (x:xs) = dropN (n-1) xs
-- COSTO LINEAL. Realiza una operacion de costo constante 

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
-- COSTO LINEAL. takeN es una operacion de costo lineal al igual que dropN

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
-- COSTO LINEAL. Aplica por cada elemento una operacion de costo constante 

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                then xs
                else x : sacar n xs
-- COSTO LINEAL. 

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =    let m = minimo xs
                in m : ordenar (sacar m xs)
-- COSTO CUADRATICO. Por cada elemento de la lista xs, lo almacena en m y aplica una funcion de costo lineal sobre el resto.

-- EJERCICO SETS ----------------------------------------

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     set = []
losQuePertenecen (x:xs) set = if belongs x set 
                              then x : losQuePertenecen xs set 
                              else losQuePertenecen xs set 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (setConElementos xs)  

setConElementos :: Eq a => [a] -> Set a 
setConElementos []     = emptyS
setConElementos (x:xs) = addS x (setConElementos xs)

unirTodos :: Eq a => Tree (Set a) -> Set a 
unirTodos EmptyT          = emptyS 
unirTodos (NodeT l ni nd) = unionS l (unionS (unirTodos ni) (unirTodos nd))

{- Funcion     SetV1  SetV2
   emptyS      O(1)    
   addS        O(n)
   belongs     O(n)
   sizeS       O(1)
   removeS     O(n)
   unionS      O(n^2)
   setToList   O(1)
-}
