
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

-- Ejercicio 3.1

data Dir = Norte | Sur | Este | Oeste
    deriving Show

--A 
opuesto :: Dir -> Dir 
opuesto Norte = Sur 
opuesto Este = Oeste
opuesto Sur = Norte 
opuesto Oeste = Este 

--B 
iguales :: Dir -> Dir -> Bool
iguales Norte Norte  = True
iguales Este Este    = True
iguales Sur Sur      = True 
iguales Oeste Oeste  = True 
iguales _ _ = False

--C 
siguiente :: Dir -> Dir 
siguiente a = 
    case a of 
    Norte -> Este 
    Este -> Sur
    Sur -> Oeste 
    Oeste -> Norte

-- Ejercicio 3.2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 
    deriving Show

primerDiaDeSemana = Lunes
ultimoDiaDeSemana = Domingo

--A 
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDiaDeSemana, ultimoDiaDeSemana)

--B 
empiezaConM :: DiaDeSemana -> Bool 
empiezaConM Martes = True 
empiezaConM Miercoles = True 
empiezaConM _ = False

--C 
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues a b =  iguales ((siguienteDia a ) b)


igualesDia :: DiaDeSemana -> DiaDeSemana -> Bool
igualesDia Lunes Lunes          = True
igualesDia Martes Martes        = True
igualesDia Miercoles Miercoles  = True 
igualesDia Jueves Jueves        = True 
igualesDia Viernes Viernes      = True 
igualesDia Sabado Sabado        = True
iguales _ _ = False


siguienteDia :: DiaDeSemana -> DiaDeSemana 
siguienteDia a = 
    case a of 
    Lunes -> Martes 
    Martes -> Miercoles
    Miercoles -> Jueves
    Jueves -> Viernes
    Viernes -> Sabado 
    Sabado -> Domingo 
    Domingo -> Lunes