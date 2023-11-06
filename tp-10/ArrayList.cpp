#include "ArrayList.h"
using namespace std;
#include <iostream>
#include <cstddef> 

struct ArrayListSt {
    int cantidad;
    int* elementos; // DUDA, ACA VOY A RECIBIR UN PUNTERO A INT, COMO SE SI ES UN ARRAY?
    int capacidad;
};
/*INVARIANTES DE REPRESENTACION: 
Capacidad >= cantidad
cantidad representa el numero de elementos en el array con valor. */

ArrayList newArrayList() {
    ArrayList array = new ArrayListSt;
    array->cantidad = 0;
    array->elementos = new int[16];
    array->capacidad = 16;
    return array;
}

ArrayList newArrayListWith(int capacidad) {
    ArrayList array;
    array->cantidad = 0;
    array->elementos = new int[capacidad];
    array->capacidad = capacidad;
    return array;
}

int lengthAL(ArrayList xs) {
    return xs->cantidad;
}

int get(int i, ArrayList xs) {
    int elemento = xs->elementos[i- 1];
    return elemento;
}

void set(int i, int x, ArrayList xs) {
    if (xs->cantidad >= i) {
        xs->elementos[i-1] = x;
    } 
}

void reescribirDecrementado(int x, ArrayList xs) {
    int* temp = xs->elementos; 
    int* nuevosElementos = new int[x];
    int i = 0;
    for (i; i < x || i < xs->cantidad; i++) {
        nuevosElementos[i] = xs->elementos[i];
    }
    delete temp;
    xs->capacidad = x;
    xs->cantidad = i;
    xs->elementos = nuevosElementos;
}

void aumentarTamano(int x, ArrayList xs) {
    int* nuevosElementos = new int[x];
    for (int i = 0; i < xs->cantidad; i++) {
        nuevosElementos[i] = xs->elementos[i];
    }
    delete xs->elementos; // DUDA? CUANDO ELEMINO ACA, ELEMINO A LO QUE APUNTA ESTO?
    xs->capacidad = x;
    xs->elementos = nuevosElementos;
}

void resize(int capacidad, ArrayList xs) {
    if (capacidad < xs->capacidad) {
        reescribirDecrementado(capacidad, xs);
    } else {
        aumentarTamano(capacidad, xs);
    }
}

void add(int x, ArrayList xs) {
    if (xs->cantidad >= xs->capacidad) {
        aumentarTamano(xs->capacidad * 2, xs);    
    }
    int posicionAAgregar = xs->cantidad;
    xs->elementos[posicionAAgregar] = x;
    xs->cantidad++;
}

void remove(ArrayList xs) {
    int posicionABorrar = xs->cantidad;
    xs->elementos[posicionABorrar] = -1;
    xs->cantidad--;
}