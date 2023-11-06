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

//Reemplaza el iesimo elemento por otro dado
void set(int i, int x, ArrayList xs);

//Decrementa o aumenta la capacidad del array 
// En caso de decrementarla se pierden los elementos del final de la lista
void resize(int capacidad, ArrayList xs);

void add(int x, ArrayList xs);

void remove(ArrayList xs);
