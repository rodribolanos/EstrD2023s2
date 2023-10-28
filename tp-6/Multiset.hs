module Multiset(Multiset, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where 
data Multiset a = MS (Map a v)

emptyMS :: Multiset a
addMS :: Ord a => a -> Multiset a -> Multiset a 
ocurrencesMS :: Ord a => a -> Multiset a -> Int 
unionMS :: Ord a => Multiset a -> Multiset a -> Multiset a 
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
multiSetToList :: MultiSet a -> [(a, Int)]

emptyMS = MS (emptyM)
addMS x (MS map) = MS (assocM x ((valorDe x map)+1) map) -- Costo O(n) + (O(n) + O(n)) = O(n)
ocurrencesMS x (MS map) = valorDe x map
unionMS (MS map1) (MS map2) = MS (asociarTodos (elementos map1) map2)
intersectionMS (MS map1) (MS map2) = MS (intersectionMap map1 map2)



valorDe :: Ord a => a -> Map a v -> Int 
valorDe x map = if esValor(lookUp x map) 
                then fromJust(lookUp x map)
                else 0
-- Costo O(n) + O(n)

esValor :: Maybe a -> Bool 
esValor Nothing = False 
esValor Maybe x = True

asociarTodos :: Ord a => [(a,v)] -> Map a v 
asociarTodos []          map2 = map2
asociarTodos ((k,v):kvs) map2 = assocM k v (asociarTodos kvs map2) 
-- Costo O(n2), siendo n la cantidad de elementos en la lista dada. 

elementos :: Ord a => Map a v -> [(a,v)]
elementos map = valoresDe (keys map)
-- Costo O(n) * O(n2), siendo n la cantidad de claves en el map.

valoresDe :: Ord a => [a] -> Map a v -> [(a,v)]
valoresDe []     map = []
valoresDe (k:ks) map = (k, (lookUp k map)) : (valoresDe ks map)
-- Costo O(n2), siendo n la cantidad de claves dada en la lista, que por precondicion forman parte del map.


intersectionMap :: Ord a => Map a v -> Map a v -> Map a v 
intersectionMap map1 map2 = asociarTodas (keys map1) map1 map2

asociarTodas :: Ord a => [a] -> Map a v -> Map a v -> Map a v 
-- PRECONDICION: Las claves dadas forman todas parte del primer map.
asociarTodas []     map1 map2 = emptyM 
asociarTodas (k:ks) map1 map2 = if esValor(k map2) 
                                then assocM k ((valorDe k map1) + (valorDe k map2)) (asociarTodas ks map1 map2)
                                else asociarTodas ks map1 map2 
-- En peor caso es O(n**2), siendo n la cantidad de claves de la lista, por cada clave realiza la operacion esValor del TAD Map k v, de costo O(n)