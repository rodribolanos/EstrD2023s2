#ifndef _UFSET_H
#define _UFSET_H

#include <iostream>
#include "Equipo.h"

using namespace std;

/*
 * UFSet.h define la representación del tipo UFSet (utilizado
 * para modelar tanto los elementos de un UFSet como el
 * conjunto en sí), y la interfaz de un UFSet
 */

#define ELEM_TYPE Equipo

struct UFNode;
typedef UFNode* UFSet;

/* Inicializa el UFSet ufset, cuyo valor asociado será value */
UFSet createUFS(ELEM_TYPE value);

/* Encuentra el elemento distinguido para el UFSet dado */
UFSet findUFS(UFSet elem);

/* Calcula la unión entre los conjuntos ufset1 y ufset2 */
void unionUFS(UFSet ufset1, UFSet ufset2);

/* Devuelve el valor asociado a elemento de tipo UFSet */
ELEM_TYPE elemUFS(UFSet ufset);

#endif


