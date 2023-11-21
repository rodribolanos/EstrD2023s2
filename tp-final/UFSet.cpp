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
   UFNode* parent;                  // Puntero a los UFNode con los que comparte conjunto 
   int rank;                        // El rango del UFNode, es la longitud de la maxima rama de UFSet´s que lo apuntan          
};

/* INVARIANTE DE REPRESENTACION: 
   En caso de un UFNode z ser su representante, z->parent = *z 
   parent tiene SIEMPRE un valor, no puede ser NULL.
   En caso de z->parent != *z, un recorrido a traves de z->parent, en ningun momento es = *z

   rank >=0 
*/

/*OBSERVACION: En la siguiente implementacion se provee una funcion unionUFS, que esta optimizada con la tecnica 
union por rango, que une al UFSet con menor rango al de mayor, ya que realizar para luego realizar un find, el costo es menor.
La implementacion no es solida al momento de disminuir el rango al momento de hacer un find y modificar el UFSet, ya que un UFSet 
NO conoce a quienes lo apuntan, por lo que va a ser imposible determinar su nuevo rango. */
/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   UFNode* ufs  = new UFNode;
   ufs->element = value; 
   ufs->parent  = ufs;
   ufs->rank    = 0;
   return ufs;
}
void actualizarPadre(UFSet r1, UFSet r2) {
// PROPOSITO: Actualiza el padre de r2, que pasa a apuntar a r1.
// PRECONDICION: El rango de r1 es MAYOR al rango de r2    
    r2->parent = r1;
}

UFSet findUFS(UFSet elem) {
   UFNode* raiz = elem;                     // Copio el puntero que me ofrecieron como si fuese su propio padre
   while (raiz->parent != raiz) {           // Bucle buscando quien es el padre del UFSet otorgado
      raiz = raiz->parent;
   }   
   // Recursion iterativa para hacer que todos los elementos por encima del otorgado, apunten a la raiz 
   // La raiz ya se encontro, este codigo es la compresion de camino.
   UFNode* hoja = elem;
   int numero = 1;
   while (hoja != raiz) {     //Si el padre del UFNode* actual hoja es igual a la raiz, significa que se procesaron todos los elementos de por medio
      UFNode* proximo = hoja->parent; // Almaceno el proximo puntero para que no haya memory leak                  
    // No tocamos el rank de hoja en este momento, ya que todos los que lo apuntan, seguiran apuntandolo.            
      hoja->parent = raiz;
    
      hoja = proximo;                 // Se pasa a iterar sobre el prox elemento del UFSet.
   }
   return raiz;
   }

UFSet elMayorEntre(UFSet r1, UFSet r2) {
    // PROPOSITO: Indica el UFSet con mayor rango entre r1 y r2
    // PRECONDICION: El rango de r1 y r2 no es igual 
    if (r1->rank > r2->rank) {
        return r1;
    } else {
        return r2;
    }
}

UFSet elMenorEntre(UFSet r1, UFSet r2) {
    // PROPOSITO: Indica el UFSet con mayor rango entre r1 y r2
    // PRECONDICION: El rango de r1 y r2 no es igual 
    if (r1->rank > r2->rank) {
        return r2;
    } else {
        return r1;
    }
}

void actualizarUnionRango(UFSet raiz1, UFSet raiz2) {
    // Sin optimizacion de rango: raiz2->parent = raiz1
    if (raiz1->rank == raiz2->rank){
      raiz2->parent = raiz1;          // En caso que los rangos sean iguales, el 2 apunta al 1. Y el rango de 1 suma 1.
      raiz1->rank++;                  // A su vez, como sus rangos son iguales, el padre tiene que aumentar en 1 el rango
   } else {  
    // En caso de no ser iguales, actualiza el padre entre el de mayor rango y el de menor entre las dos raices.
    actualizarPadre(elMayorEntre(raiz1, raiz2), elMenorEntre(raiz1,raiz2));
    }
}
/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */
void unionUFS(UFSet ufset1, UFSet ufset2) {
   UFNode* raiz1 = findUFS(ufset1);
   UFNode* raiz2 = findUFS(ufset2);
   if (raiz1 != raiz2) {
    actualizarUnion(raiz1, raiz2);
   }
}

/* Devuelve el valor asociado a elemento de tipo UFSet */
ELEM_TYPE elemUFS(UFSet ufset) {
   return ufset->element;
}
