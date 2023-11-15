#include "UFSet.h"

/*
 * UFSet.cpp contiene la implementación de la interfaz para UFSet declarada en UFSet.h. 
 * Deben implementarse las operaciones de acuerdo a la representación elegida para el tipo UFSet.
 */

/* El tipo UFNode* representa:
 *  1. un elemento de un UFSet (o sea, un nodo del árbol que contiene a todos los elementos del conjunto)
 *  2. al conjunto en su totalidad, si el nodo es la raíz del arbol
 *
 *  El nodo tiene un puntero a su elemento asociado en el campo element. 
 *  Deberán agregarse los campos necesarios para completar la representación.
 */
struct UFNode {
   ELEM_TYPE element;               // Elemento asociado al nodo. En el caso de nuestro TP, sera Equipo.
   UFNode* parent;         // Puntero a los UFNode con los que comparte conjunto 
   int rank;
};

/* INVARIANTE DE REPRESENTACION: 
   En caso de un UFNode z ser la raiz del arbol, parent = *z 
   parent tiene SIEMPRE un valor, no puede ser NULL.

   rank >=0 
*/

/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   UFNode* ufs = new UFNode;
   ufs->element = value; 
   ufs->parent = ufs;
   return ufs;
}


UFSet findUFS(UFSet elem) {
   UFNode* raiz = elem;                     // Copio el puntero que me ofrecieron como si fuese su propio padre
   int elementosRecorridos = -1;             // La cantidad de elementos que estan por encima de si mismo, empieza en -1 ya
                                             // que no tomaremos en cuenta al padre. 
   while (raiz->parent != raiz) { // Bucle buscando quien es el padre del UFSet otorgado
      raiz = raiz->parent;
      elementosRecorridos = elementosRecorridos + 1;
   }   
   // Recursion iterativa para hacer que todos los elementos por encima del otorgado, apunten a la raiz 
   // La raiz ya se encontro, este codigo es la compresion de camino.
   UFNode* hoja = elem;
   while (hoja->parent != raiz) {     //Si el padre del UFNode* actual hoja es igual a la raiz, significa que se procesaron todos los elementos de por medio
      UFNode* proximo = hoja->parent; // Almaceno el proximo puntero para que no haya memory leak
      hoja->parent = raiz;            // El elemento actual apunta a la raiz
      // No tocamos el rank de hoja en este momento, ya que todos los que lo apuntan, seguiran apuntandolo.                      
      hoja = proximo;                         // Se pasa a iterar sobre el prox elemento del UFSet.
      
   }
   return raiz;
}


/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */
void unionUFS(UFSet ufset1, UFSet ufset2) {
   UFNode* raiz1 = findUFS(ufset1);
   UFNode* raiz2 = findUFS(ufset2);
   if (raiz1->rank > raiz2->rank) {
      raiz2->parent = raiz1;    // En caso del rango del padre de UFSet1, ser mayor al 2, hacemos que el 2 apunte al 1
   } else if (raiz1->rank == raiz2->rank){
      raiz2->parent = raiz1;    // En caso que los rangos sean iguales, el 2 apunta al 1. Y el rango de 1 suma 1.
      raiz1->rank++;             
   } else {
      raiz1->parent = raiz2;    // En caso del rango del padre de UFSet2, ser mayor al 1, hacemos que el 1 apunte al 2
   }
}

/* Devuelve el valor asociado a elemento de tipo UFSet */
ELEM_TYPE elemUFS(UFSet ufset) {
   return ufset->element;
}
