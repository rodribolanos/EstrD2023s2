module QueueV2 (Queue ,emptyQ, isEmptyQ, enqueue, firstQ, dequeue) 
where 
data Queue a = Q [a] 
    
emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q l)  = null l

enqueue :: a -> Queue a -> Queue a
enqueue x (Q l) = (Q (listaQDeAgregar x l)) 

firstQ :: Queue a -> a
-- LA COLA TIENE AL MENOS UN ELEMENTO 
firstQ (Q l) = ultimoDe l  

dequeue :: Queue a -> Queue a
-- LA COLA TIENE AL MENOS UN ELEMENTO
dequeue (Q l) = Q (listaSinElUltimo l)  


listaQDeAgregar :: a -> [a] -> [a]
listaQDeAgregar x ys = x : ys

ultimoDe :: [a] -> a
ultimoDe []     = error"La lista no contiene elementos"
ultimoDe (x:xs) = if null xs 
                  then x 
                  else ultimoDe xs

listaSinElUltimo :: [a] -> [a]
listaSinElUltimo []     = []
listaSinElUltimo (x:xs) = if null xs 
                          then []   
                          else x : listaSinElUltimo xs 