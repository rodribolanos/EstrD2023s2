module Set 
(nuevoS, addS, belongs, sizeS, removeS, unionS, setToList)
where


data Set a = S [a]          Int 
--                 Coleccion    Size de coleccion
{- Siendo Set a = S [a] n
INV. REP.: n tiene siempre el mismo valor que elementos de la lista.
           En caso de la lista ser vacia, n = 0                                       -}
nuevoS :: Set a
nuevoS = S [] 0
-- COSTO CONSTANTE

addS :: Eq a => a -> Set a -> Set a
addS m (S ys n) = (S (agregarElemento m ys) (n+1))
-- COSTO LINEAL. Realiza una operacion de costo lineal en su codigo.

agregarElemento :: Eq a => a -> [a] -> [a]
agregarElemento x ys = if elem x ys   
                           then ys 
                           else x:ys
-- COSTO LINEAL. Usa elem, que por cada elemento de la lista realiza una operacion de costo constante    

belongs :: Eq a => a -> Set a -> Bool
belongs x (S ys n) = elem x ys 
-- COSTO LINEAL. Usa elem, que por cada elemento de la lista realiza una operacion de costo constante

sizeS :: Eq a => Set a -> Int
sizeS (S ys n) = n 
-- COSTO CONSTANTE 

removeS :: Eq a => a -> Set a -> Set a
-- PRECONDICION: El elemento a eliminar pertenece a la lista del Set 
removeS x (S ys n) = (S (listaSin x ys) (n-1))

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs n1) (S ys n2) = (S (listasSinRepetir xs ys) (size (listasSinRepetir xs ys)))

listasSinRepetir :: Eq a => [a] -> [a] -> [a] 
listasSinRepetir []     ys   = ys 
listasSinRepetir (x:xs) ys   = if x elem ys 
                              then listasSinRepetir xs ys
                              else x : listasSinRepetir xs ys

listaSin :: Eq a => a -> [a] -> [a]
listaSin x []     = 
listaSin x (y:ys) = if x == y 
                    then ys
                    else y : listaSin x ys   