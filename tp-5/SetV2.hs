module SetV2
(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where


data Set a = S [a]          

emptyS :: Set a 
emptyS = S []
-- CONSTANTE
addS :: Eq a => a -> Set a -> Set a 
addS x (S ys) = S (x:ys)
-- CONSTANTE
belongs :: Eq a => a -> Set a -> Bool
belongs x (S ys) = elem x ys 
-- LINEAL
sizeS :: Eq a => Set a -> Int
sizeS set = length (setToList set)
-- 
removeS :: Eq a => a -> Set a -> Set a 
-- PRECONDICION: El set contiene el elemento dado 
removeS x (S ys) = S (listaSin x ys)
-- LINEAL
listaSin :: Eq a => a -> [a] -> [a]
listaSin x []     = []
listaSin x (y:ys) = if x == y 
                    then ys
                    else y : listaSin x ys

unionS :: Eq a => Set a -> Set a -> Set a 
unionS (S xs) (S ys) = S (xs ++ ys)
-- LINEAL

setToList :: Eq a => Set a -> [a]
setToList (S xs) = sinRepetidos xs 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =   if elem x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs