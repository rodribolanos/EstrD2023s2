module QueueV3
(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) 
where 
data Queue a = Q [a] [a]
{- Siendo Queue a = Q fs bs 
    -}

emptyQ :: Queue a 
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool 
isEmptyQ (Q fs bs) = null fs 

enqueue :: a -> Queue a -> Queue a 
enqueue x (Q fs bs) = Q fs (x:bs)

firstQ :: Queue a -> a 
firstQ (Q fs bs) = head fs

dequeue :: Queue a -> Queue a 
dequeue (Q fs bs) = 