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

ejM :: Map Int String
ejM = assocM 10 "Numero diez"
      $ assocM 20 "Numero veinte"
      $ assocM 30 "Numero treinta"
      $ emptyM