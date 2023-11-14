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
   UFNode* equipoAsociado;         // Puntero a los UFNode con los que comparte conjunto 
};

/* INVARIANTE DE REPRESENTACION: 
   En caso de UFNode ser la raiz del arbol, equipoAsociado = NULL 
   equipoAsociado no puede apuntar a un UFNode con el mismo valor en element. 
*/

/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   UFNode* ufs = new UFNode;
   ufs->element = value; 
   ufs->equipoAsociado = ufs;
   return ufs;
}

/*
 * Encuentra el elemento distinguido para el UFSet dado. 
 * Esta operación puede ser optimizada con la técnica de compresión de camino.
 */
UFSet findUFS(UFSet elem) {
   if (elem->equipoAsociado == elem) { // Si el padre es si mismo, significa que el UFSet otorgado es su propio padre.
      return elem;
   } else {   
   while (elem->equipoAsociado != elem) { //Si el padre del UFSet actual es si mismo, significa que es su propio padre
      elem = elem->equipoAsociado;        // Sino, lo actualiza
   }
   return elem;
   }
   // O(N)
}

/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */
void unionUFS(UFSet ufset1, UFSet ufset2) {
   UFNode* raiz1 = findUFS(UFSet ufset1);
   UFNode* raiz2 = findUFS(UFSet ufset2);
   raiz1->equipoAsociado = raiz2;
}

/* Devuelve el valor asociado a elemento de tipo UFSet */
ELEM_TYPE elemUFS(UFSet ufset) {
   
}
