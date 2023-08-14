
-- Ejercicio 2.1
--A
sucesor :: Int -> Int
sucesor a = a+1

--B
sumar :: Int -> Int -> Int
sumar a b = a + b 

--C
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = (div a b , mod a b)

--D 
maxDelPar :: (Int, Int) -> Int
maxDelPar (a, b) = if a > b 
                   then a 
                   else b 

-- Ejercicio 2.2
{- Ejemplos de expresiones que denoten el numero 10, utilizando en cada expresion todas las funciones del punto anterior 
    1- sumar (maxDelPar (divisionYResto 10 2)) (sucesor 4)
    2- sucesor (sumar (maxDelPar (divisionYResto 10 3)) 6)
    3- maxDelPar (divisionYResto (sumar 50 50) (sucesor 9))
    4- sumar (maxDelPar (divisionYResto 100 20)) (sucesor 4)
-}

-- Ejercicio 3

data Dir = Norte | Sur | Este | Oeste
    deriving Show

--A 
opuesto :: Dir -> Dir 
opuesto Norte = Este 
opuesto Este = Sur 
opuesto Sur = Oeste 
opuesto Oeste = Norte 
