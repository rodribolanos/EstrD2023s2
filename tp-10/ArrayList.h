struct ArrayListSt;
typedef ArrayListSt* ArrayList;

//Crea una lista con 0 elementos
// Empieza con 16 de capacidad
ArrayList newArrayList();

//Crea una lista con 0 elementos y una capacidad dada por parametro
ArrayList newArrayListWith(int capacidad);

//Devuelve la cantidad de elementos existentes
int lengthAl(ArrayList xs);

//Devuelve en iesimo elemento de la lista
int get(int i, ArrayList xs);

//Reemplaza 
