data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

treeBST =   NodeT 10 
        (NodeT 8 
            (NodeT 2 
                (NodeT 1 EmptyT EmptyT) 
                EmptyT) 
            EmptyT)    
        (NodeT 12 
            (NodeT 11 EmptyT EmptyT) 
            EmptyT)
belongsBST :: Ord a => a -> Tree a -> Bool 
belongsBST y EmptyT          = False 
belongsBST y (NodeT x ti td) = y == x || belongsBST y (treeADirigirse y x ti td) 
-- costo O (log n), siendo N la cantidad de elementos del arbol, ya que solo se recorre la rama donde se puede encontrar la Y 

treeADirigirse :: Ord a => a -> a -> Tree a -> Tree a -> Tree a 
treeADirigirse x y ti td = if x < y 
                           then td 
                           else ti 

insertBST :: Ord a => a -> Tree a -> Tree a 
insertBST x EmptyT          = (NodeT x EmptyT EmptyT)  
insertBST x (NodeT y ti td) = if x == y 
                              then NodeT x ti td 
                              else if x > y 
                              then (NodeT y ti (insertBST x td))
                              else (NodeT y (insertBST x ti) td)

deleteBST :: Ord a => a -> Tree a -> Tree a 
deleteBST x (EmptyT)        = EmptyT 
deleteBST x (NodeT y ti td) = if x == y 
                              then rearmarBST ti td 
                              else if x > y 
                              then NodeT y ti (deleteBST x td)
                              else NodeT y (deleteBST x ti) td
       
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a 
-- PRECONDICION: Los arboles son emptyT 
rearmarBST EmptyT td = td 
rearmarBST ti     td = NodeT (maxBST ti) (delMaxBST ti) td 

maxBST :: Ord a => Tree a -> a 
maxBST (NodeT x ti EmptyT) = x
maxBST (NodeT x ti td)     = maxBST td 

delMaxBST :: Ord a => Tree a -> Tree a
delMaxBST (NodeT _ ti EmptyT) = ti
delMaxBST (NodeT x ti td)     = NodeT x ti (delMaxBST td) 

splitMinBST :: Ord a => Tree a -> (a, Tree a) 
-- PRECONDICION: El arbol no es vacio y es un BST 
splitMinBST (NodeT x ti td)= (x, rearmarBST ti td)

esBST :: Ord a => Tree a -> Bool 
-- Indica si el arbol dado es BST 
esBST EmptyT          = True 
esBST (NodeT x ti td) = esElMasGrande x ti && esElMasChico x td  

esElMasGrande :: Ord a => a -> Tree a -> Bool 
esElMasGrande x EmptyT          = True 
esElMasGrande x (NodeT y ti td) = x > y && esElMasGrande x ti && esElMasGrande x td

esElMasChico :: Ord a => a -> Tree a -> Bool 
esElMasChico x EmptyT          = True 
esElMasChico x (NodeT y ti td) = x > y && (esElMasChico x ti) && (esElMasChico x td)

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a 
elMaximoMenorA x (EmptyT)            = Nothing
elMaximoMenorA x (NodeT y EmptyT td) = if x > y 
                                       then Just y 
                                       else Nothing
elMaximoMenorA x (NodeT y ti td)     = if x > y 
                                   then Just y    
                                   else elMaximoMenorA x ti    
{- 
Dada la siguiente interfaz y costos para el tipo abstracto Map:
emptyM :: Map k v
Costo: O(1).
assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).
lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).
deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).
keys :: Map k v -> [k]
Costo: O(K).

K cantidad de claves del map 
-}

listToMap :: Eq k => [(k,v)] -> Map k v -- Costo O(n log k) siendo n la cantidad de claves en la lista, y k la cantidad de claves del map
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k,v)]  -- Costo O(n log K), siendo n la cantidad de claves en la lista dada y K la cantidad de claves en el map                     
mapToList map = tuplaConValores (keys map) map

tuplaConValores :: Eq k => [k] -> Map k v -> [(k,v)] -- Costo O(n log K), siendo n la cantidad de claves en la lista dada y K la cantidad de claves en el map
tuplaConValores []     map = []
tuplaConValores (k:ks) map = (crearTuplaValor k map) : tuplaConValores ks map 

crearTuplaValor :: Eq k => k -> Map k v -> (k,v) -- O(log K), siendo k la cantidad de claves en el map dado.     
crearTuplaValor k map = (k, (valorDe k map))

valorDe :: Eq k => k -> Map k v -> v -- O(log K), siendo k la cantidad de claves en el map dado.
valorDe k map = fromJust(lookupM k map) 

agruparEq :: Eq k => [(k,v)] -> Map k [v] -- Costo (k log k), siendo k la cantidad de claves de tuplas en la lista 
agruparEq []       = emptyM
agruparEq (kv:kvs) = asociarConLista kv (agruparEq kvs)

asociarConLista :: Eq k => (k,v) -> Map k [v] -> Map k [v] -- Costo O(log K) + O(log K) + O (log K)
asociarConLista (k,v) map = if esValor(lookupM k map) 
                            then assocM k (v:(valorDe k map)) map
                            else assocM k [v] map 

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno acada número asociado con dichas claves.
incrementar []     map = map 
incrementar (k:ks) map = asociarValorIncrementado k (incrementar ks map) 
-- Costo O(n^2) siendo n la longitud de la lista de claves

asociarValorIncrementado :: Eq k => k -> Map k Int -> Map k Int 
asociarValorIncrementado k map = if esValor (lookupM k map) 
                                 then assocM k (valorDe k map + 1) map
                                 else map

 
indexar :: [a] -> Map Int a  
indexar xs   = asociarDadoVuelta (reversa xs) 

asociarDadoVuelta :: [a] -> Map Int a 
asociarDadoVuelta []     = emptyM 
asociarDadoVuelta (x:xs) = assocM (1 + longitud xs) x (asociarDadoVuelta xs) 