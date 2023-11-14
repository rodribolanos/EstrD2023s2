struct LinkedListSt;
struct IteratorSt;
struct NodoL;

typedef LinkedListSt* LinkedList; // Inv Rep: el puntero NO es NULL
typedef IteratorSt* ListIterator; // Inv Rep: el puntero NO es NULL

// Crea una linked list vacia 
LinkedList nil();

// Indica si la lista esta vacia 
bool isEmpty(LinkedList xs);

// Devuelve el primer elemento 
int head(LinkedList xs);

// Agrega un elemento al principio de la lista 
void Cons(int x, LinkedList xs);

// Quita el primer elemento 
void Tail(LinkedList xs);

// Devuelve la cantidad de elementos
int length(LinkedList xs);

// Agrega un elemento al final de la lista.
void Snoc(int x, LinkedList xs);

// Apunta el recorrido al primer elemento.
ListIterator getIterator(LinkedList xs);

// Devuelve el elemento actual en el recorrido.
int current(ListIterator ixs);

// Reemplaza el elemento actual por otro elemento.
void SetCurrent(int x, ListIterator ixs);

// Pasa al siguiente elemento.
void Next(ListIterator ixs);

// Indica si el recorrido ha terminado.
bool atEnd(ListIterator ixs);

// Libera la memoria ocupada por el iterador.
void DisposeIterator(ListIterator ixs);

// Libera la memoria ocupada por la lista.
void DestroyL(LinkedList xs);

