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
   int cantidadDeHijosRank;         // Cantidad de hijos con rango alto. (Rango alto es el rank del UFNode - 1)          
};

/* INVARIANTE DE REPRESENTACION: 
   En caso de un UFNode z ser la raiz del arbol, parent = *z 
   parent tiene SIEMPRE un valor, no puede ser NULL.
   
   cantidadDeHijosRank >= 0
   rank >=0 
*/

/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   UFNode* ufs = new UFNode;
   ufs->element = value; 
   ufs->parent = ufs;
   ufs->cantidadDeHijosRank = 0;
   return ufs;
}

void actualizarRango(UFSet r1, UFSet r2) {
// PROPOSITO: Verifica si r2 es un nuevo hijo de rango (r1->rank - 1)
// PRECONDICION: El rango de r1 es MAYOR a el rango de r2. Sino rompe invariante.
    if (r1->rank == r2->rank + 1) {
        r1->cantidadDeHijosRank++;
    } 
}

void actualizarPadre(UFSet r1, UFSet r2) {
// PROPOSITO: Actualiza el padre de r2, que pasa a apuntar a r1.
// PRECONDICION: El rango de r1 es MAYOR al rango de r2    
    r2->parent = r1;
    actualizarRango(r1,r2);
}

void verificarRango(UFSet hijo, UFSet padre, int numero) {
// PROPOSITO: Actualiza en caso de ser necesario el rango de padre.
// PRECONDICION: El rank de hijo es MENOR al rank de padre.
// OBS: numero <= 2. El numero indica la diferencia de rango entre hijo y padre
// El unico caso en que es 2, es si el hijo decremento su rango previamente 
   if (hijo->rank + numero == padre->rank) {
        padre->cantidadDeHijosRank--;
    } 
    if (padre->cantidadDeHijosRank == 0) {
        padre->rank--;
    }   
}

int numeroACambiar(UFSet hoja, UFSet proximo) {
   if (padre->cantidadDeHijosRank == 0) {
        return 2;
    } else  {
      return 1;
    }
}

void verificarRangoRaiz(UFSet hoja, UFSet raiz, int numero) {
// PROPOSITO: Verifica si la hoja fue disminuida de rango (numero == 2), si la hoja era uno de los 
// hijos de rango superior a  
   if (numero == 2 && (hoja->rank + numero == raiz->rank)) {
      raiz->cantidadDeHijosRank--;
      if (raiz->cantidadDeHijosRank == 0) {
      // En caso de que la cantidad de hijos haya sido 0 la resta anterior, significa que hay un hijo al menos 
      // del rank -2. Por lo que podriamos el rank baja en 1, y tiene al menos 1 hijo de ese rank -1  
      raiz->rank--;
      raiz->cantidadDeHijosRank = 1; 
   } 
   }
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
      if (proximo != raiz)  {
      hoja->parent = raiz;
      verificarRango(hoja, proximo, numero);  // En este momento si se cambia el rango del proximo
      numero = numeroACambiar(hoja, proximo);
}  else {
   verificarRangoRaiz(hoja, raiz, numero); // En este punto, tenemos los ultimos dos elementos. Verificaremos que la hoja (quien seguira apuntando
// a la raiz, no haya bajado su rango. En ese caso, bajaremos en 1 la cantidad de elementos que lo apuntan
} 
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
/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */
void unionUFS(UFSet ufset1, UFSet ufset2) {
   UFNode* raiz1 = findUFS(ufset1);
   UFNode* raiz2 = findUFS(ufset2);
   if (raiz1->rank == raiz2->rank){
      raiz2->parent = raiz1;          // En caso que los rangos sean iguales, el 2 apunta al 1. Y el rango de 1 suma 1.
      raiz1->rank++;                  // A su vez, como sus rangos son iguales, el padre tiene que aumentar en 1 el rango
      raiz1->cantidadDeHijosRank = 1;  // Como tuvo que aumentar el rango, significa que su hijo con mayor rango es solo 1.
   } else {  
    // En caso de no ser iguales, actualiza el padre entre el de mayor rango y el de menor entre las dos raices.
    actualizarPadre(elMayorEntre(raiz1, raiz2), elMenorEntre(raiz1,raiz2));
    }
}

/* Devuelve el valor asociado a elemento de tipo UFSet */
ELEM_TYPE elemUFS(UFSet ufset) {
   return ufset->element;
}
