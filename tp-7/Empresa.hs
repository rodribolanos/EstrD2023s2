type SectorID = Int 
type CUIL     = Int 

module Empresa (Empresa,)
where 
    data Empresa = ConsE (Map SectorID (Set Empleado)) (Map CUIL Empleado)

{- INV REP: Siendo Empresa = ConsE mss mce. 
    *Los empleados que estan presentes en los valores en mss estan tambien presentes en los valores de mce 
    *Los empleados de los valores mce estan presentes en los valores de el map mss 
    * La clave CUIL asociada al empleado en mce es el cuil del empleado
    -}

{-
consEmpleado :: CUIL -> Empleado
Propósito: construye un empleado con dicho CUIL.
Costo: O(1)

cuil :: Empleado -> CUIL
Propósito: indica el CUIL de un empleado.
Costo: O(1)

incorporarSector :: SectorId -> Empleado -> Empleado
Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.

sectores :: Empleado -> SectorId
Propósito: indica los sectores en los que el empleado trabaja.
Costo: O(1)
 -}

consEmpresa :: Empresa -- Costo O(1)
consEmpresa = ConsE emptyM emptyM

buscarPorCuil :: CUIL -> Empresa -> Empleado 
buscarPorCuil c (ConsE mss mce) = empleadoConCuil c mce

empleadoConCuil :: CUIL -> Map CUIL Empleado -> Empleado -- Costo (log E). Siendo E la cantidad de empleados en el map, lookUp realiza una operacion de costo (log E)
empleadoConCuil c mce = lookUp c mce


empleadosDelSector :: SectorID -> Empresa -> [Empleado] -- COSTO O(logS + E)
-- PRECONDICION: El sector dado esta asociado a la nave. 
empleadosDelSector s (ConsE mss mce) = setToList (empleadosEn s mss)

empleadosEn :: SectorID -> Map SectorID (Set Empleado) -> Set Empleado
empleadosEn s mss = fromJust(lookUp s mss)

todosLosCUIL :: Empresa -> [CUIL]  -- Costo O(E), siendo E la cantidad de empleados, keys es una operacion de costo O(E) del TAD Map 
todosLosCUL (ConsE mss mce) = keys (mce) 

todosLosSectores :: Empresa -> [SectorID] -- Costo O(S), siendo S la cantidad de sectores de la empresa, keys es una operacion de costo O(E) del tad Map 
todosLosSectores (ConsE mss mce) = keys mss

agregarSector :: SectorID -> Empresa -> Empresa -- Costo O (log S), siendo S la cantidad de sectores de la empresa, se utiliza la funcion assocM de costo O(S) del TAD Map
agregarSector s (ConsE mss mce) = ConsE (agregarSectorA mss) mce 

agregarSectorA :: SectorID -> Map SectorID (Set Empleado) -> Map SectorID (Set Empleado)
agregarSectorA s mss = assocM s emptyS mss

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendráel CUIL dado
agregarEmpleado []     c emp = emp 
agregarEmpleado (s:ss) c emp = agregarEmpleadoASector s c (agregarEmpleado ss c emp)

agregarEmpleado :: SectorID -> CUIL -> Empresa -> Empresa 
agregarEmpleado s c (ConsE mss mce) = ConsE (assocM )