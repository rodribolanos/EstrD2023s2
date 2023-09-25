module PriorityQueue (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where 
data PriorityQueue a = PQ [a]
{- Siendo una Priority Queue = PQ xs
 La lista xs esta ordenada segun la prioridad, siendo el primero el elemento con mas prioridad 
 de la cola-}
emptyPQ :: PriorityQueue a
-- Devuelve una priority queue vacia 
isEmptyPQ :: PriorityQueue a -> Bool
-- Indica si la priority queue dada esta vacia  
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- Inserta el elemento dado a la priority queue. 
findMinPQ :: Ord a => PriorityQueue a -> a
-- Devuelve el elemento con mayor prioridad en la priority queue  
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- Elimina el elemento con mayor prioridad de la priority queue  

emptyPQ             = PQ []            -- O(1)
isEmptyPQ (PQ xs)   = null xs          -- O(1)
insertPQ x (PQ xs)  = PQ (agregar x xs) -- O(n)  -- n es la cantidad de elementos de la pq
findMinPQ (PQ xs)   = head xs           -- O(1)
deleteMinPQ (PQ xs) = PQ (tail xs)      -- O(1)

agregar :: Ord a => a -> [a] -> [a]
agregar x []     = [x]
agregar x (y:ys) = if x < y 
                   then x:(y:ys)
                   else y : (agregar x ys)
 