module MapV2 (Map, emptyM, assocM, lookupM, deleteM, keys)
where 
data Map k v = M [(k,v)]

emptyM :: Map k v 
assocM :: Eq k => k -> v -> Map k v -> Map k v 
lookupM :: Eq k => k -> Map k v -> Maybe v 
deleteM :: Eq k => k -> Map k v -> Map k v 
keys :: Eq k => Map k v -> [k]

{-Map con lista de pares clave-valor con repetidos-}

emptyM = M [] -- O(1)
assocM k v (M kvs) = M ((k,v) : kvs) -- O(1)
lookupM k (M kvs) = valorDe k kvs -- Costo O(N)
deleteM k (M kvs) = M (sinClave k kvs) -- Costo O(N)
keys (M kvs) = clavesSinRepetir kvs 

valorDe :: Eq k => k -> [(k,v)] -> Maybe v -- Costo O(N). Siendo n la cantida de pares en la lista 
valorDe k []       = Nothing 
valorDe k ((k1,v1):kvs) = if k == k1 
                     then Just v1
                     else valorDe k kvs

sinClave :: Eq k => k -> [(k,v)] -> [(k,v)] -- Costo O(N). Siendo N la cantidad de pares en la lista. 
sinClave k []          = []
sinClave k ((k1,v1):kvs) = if k == k1 
                         then kvs  
                         else (k1,v1) : (sinClave k kvs)



clavesSinRepetir :: Eq k =>[(k,v)] -> [k]
clavesSinRepetir []          = []
clavesSinRepetir ((k,v):kvs) = if elem k (clavesSinRepetir kvs) 
                     then clavesSinRepetir kvs 
                     else k:(clavesSinRepetir kvs)
-- Costo O(n^2). Por cada clave se consulta si pertenece a la lista de la propia recursion. 



 



