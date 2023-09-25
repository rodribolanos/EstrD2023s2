module Map (Map, emptyM, assocM, lookupM, deleteM, keys)
where 
data Map k v = M [(k,v)] 
{- INV. REP. Siendo un Map k v = M [(k,v)]  
         No existen dos claves iguales en la lista de pares
         -}
    
emptyM :: Map k v 
assocM :: Eq k => k -> v -> Map k v -> Map k v 
lookupM :: Eq k => k -> Map k v -> Maybe v 
deleteM :: Eq k => k -> Map k v -> Map k v 
keys :: Map k v -> [k] 

emptyM             = M []                     -- Costo O(1)
assocM k v (M kvs) = M (agregarClave k v kvs) -- Costo O(n) siendo n la cantidad de elementos en kvs
lookupM k (M kvs)  = buscar k kvs             -- Costo O(n) siendo n la cantidad de elementos en kvs
deleteM k (M kvs)  = M (borrar k kvs)         -- Costo O(n) siendo n la cantidad de elementos en kvs
keys (M kvs)       = claves kvs               -- Costo O(n) siendo n la cantidad de elementos en kvs
 
agregarClave :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
agregarClave k v []            = [(k,v)]
agregarClave k v ((k2,v2):kvs) = if k==k2
                                 then (k2,v):kvs -- Uno el nuevo valor a la clave existente. 
                                 else (k2,v2) : (agregarClave k v kvs)   

claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs

borrar :: Eq k => k -> [(k,v)] -> [(k,v)] 
borrar k []            = []
borrar k ((k1,v1):kvs) = if k == k1 
                         then kvs 
                         else (k1,v1) : borrar k kvs

buscar :: Eq k => k -> [(k,v)] -> Maybe v 
buscar k []             = Nothing
buscar k ((k1,v1): kvs) = if k == k1 
                          then Just v1
                          else buscar k kvs