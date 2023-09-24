module StackV1 (
    Stack, emptyStack, isEmptyStack, push, top, pop, lenS
) where

data Stack a = Stack [a] Int

{--Inv Rep: -- Stack xs size
               size representa la cantidad de elementos de xs
--}
emptyStack :: Stack a
--Crea una pila vacía.
emptyStack = Stack [] 0

isEmptyStack :: Stack a -> Bool
--Dada una pila indica si está vacía.
isEmptyStack (Stack xs s) = s == 0

push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
push e (Stack xs s) = Stack (e:xs) (s + 1)

top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
--La pila tiene al menos un elemento.
top (Stack xs s) = head xs

pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
--La pila tiene al menos un elemento.
pop (Stack xs s) = Stack (tail xs) (s-1)

lenS :: Stack a -> Int
--Da la cantidad de elementos en la pila.
--Costo: constante.
lenS (Stack xs s) = s