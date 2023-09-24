module QueueV1 
(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) 
where 
data Queue a = Q [a]

emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q l)  = null l

enqueue :: a -> Queue a -> Queue a
enqueue x (Q l) = Q (listaQDeAgregar x l) 

firstQ :: Queue a -> a
-- LA COLA TIENE AL MENOS UN ELEMENTO 
firstQ (Q l) = head l 

dequeue :: Queue a -> Queue a
-- LA COLA TIENE AL MENOS UN ELEMENTO
dequeue (Q l) = Q (tail l) 

listaQDeAgregar :: a -> [a] -> [a]
listaQDeAgregar x ys = ys ++ [x]