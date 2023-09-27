import PriorityQueue 
import Map 

instance (Show a, Ord a) => Show (PriorityQueue a) where 
    show pq = if isEmptyPQ pq 
              then "<<"
              else show (findMinPQ pq) ++ "<<" ++ show (deleteMinPQ pq) 

ejPQ = insertPQ 5
      $ insertPQ 4
      $ insertPQ 3
      $ insertPQ 10
      $ emptyPQ

heapSort :: Ord a => [a] -> [a]
heapSort xs = pq2List (list2PQ xs)

list2PQ :: Ord a => [a] -> PriorityQueue a 
list2PQ []     = emptyPQ                   -- O(1)
list2PQ (x:xs) = insertPQ x (list2PQ xs)   -- O(n)
-- COSTO: O(n^2). Por cada elemento se realiza una operacion de costo lineal
pq2List :: Ord a => PriorityQueue a -> [a]
pq2List pq = if isEmptyPQ pq
             then []
             else findMinPQ pq : (pq2List (deleteMinPQ pq))
-- COSTO O(n). Siendo n la cantidad de elementos de la pq. Por cada elemento se realiza una 
-- operacion de costo costante

instance (Eq k, Show k, Show v) => Show (Map k v) where 
    show map = showAssocs (keys map) map where 
            showAssocs []  map    = "<emptyM>"
            showAssocs [k] map    = showAssoc k map 
            showAssocs (k:ks) map = showAssoc k map ++ "," ++ showAssocs ks map

showAssoc k map = show k ++ "<-" ++ show (fromJust (lookupM k map))
fromJust (Just m) = m

ejM :: Map Int Int
ejM = assocM 10 10
      $ assocM 20 20
      $ assocM 30 30
      $ emptyM

values :: Eq k => Map k v -> [Maybe v] -- Costo O(n^2) Siendo n la cantidad de claves del map.
values map = valoresDe (keys map) map   

valoresDe :: Eq k => [k] -> Map k v -> [Maybe v]
valoresDe []     map = []
valoresDe (k:ks) map = (lookupM k map) : valoresDe ks map  -- Costo O(n^2) Siendo n la cantidad de claves del map.

todasAsociadas :: Eq k => [k] -> Map k v -> Bool  -- Costo O(n^2) Siendo n la cantidad de claves de la lista.
-- Indica si en el map se encuentran todas las claves dadas
todasAsociadas []     map = True
todasAsociadas (k:ks) map = estaAsociada k map && todasAsociadas ks map 

estaAsociada :: Eq k => k -> Map k v -> Bool
estaAsociada k map = esValor (lookupM k map)

esValor :: Maybe v -> Bool
esValor Nothing  = False 
esValor (Just x) = True

listToMap :: Eq k => [(k,v)] -> Map k v -- Costo O(n^2) siendo n la cantidad de claves en el map.
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k,v)]
mapToList map = tuplaConValores (keys map) map

tuplaConValores :: Eq k => [k] -> Map k v -> [(k,v)]
tuplaConValores []     map = []
tuplaConValores (k:ks) map = (crearTuplaValor k map) : tuplaConValores ks map

crearTuplaValor :: Eq k => k -> Map k v -> (k,v)
crearTuplaValor k map = (k, (valorDe k map))

valorDe :: Eq k => k -> Map k v -> v 
valorDe k map = fromJust(lookupM k map)

agruparEq :: Eq k => [(k,v)] -> Map k [v]
agruparEq []       = emptyM
agruparEq (kv:kvs) = asociarConLista kv (agruparEq kvs)

asociarConLista :: Eq k => (k,v) -> Map k [v] -> Map k [v] 
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


 