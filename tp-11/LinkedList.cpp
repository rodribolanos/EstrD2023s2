using namespace std;
#include <iostream>
#include "LinkedList.h"

LinkedList nil() {
    LinkedListSt* list = new LinkedListSt;
     list->cantidad = 0;
     list->primero  = NULL;
     return list;
     // O(1) 
}

bool isEmpty(LinkedList xs) {
    return xs->cantidad == 0;
    // O(1) 
}

int head(LinkedList xs) {
    return xs->primero->elem;
    // O(1) 
}

void Cons(int x, LinkedList xs) {
    NodoL* nodo = new NodoL;
    nodo->elem =  x; 
    nodo->siguiente = xs->primero;
    xs->cantidad++;
    xs->primero = nodo;
    // O(1) 
}

void Tail(LinkedList xs) {
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    xs->cantidad--;
    delete temp;
    // O(1) 
}

int length(LinkedList xs) {
    return xs->cantidad;
    // O(1) 
}

void Snoc(int x, LinkedList xs) {
    NodoL* nodo = new NodoL;
    nodo->elem =  x; 
    nodo->siguiente = NULL;
    NodoL* actual = xs->primero; 
    if (actual == NULL) {
        xs->primero = nodo;
        xs->cantidad++;
    } else {
        while(actual->siguiente != NULL) {
            actual = actual->siguiente;
        } // Aca corto el while 
        actual->siguiente = nodo;
    }
    xs->cantidad++;
    // O(N), siendo N la cantidad de elementos en la lista  
}

ListIterator getIterator(LinkedList xs) {
    IteratorSt* iterator = new IteratorSt; 
    iterator->current = xs->primero;
    return iterator;
}

int current(ListIterator ixs) {
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs) {
    ixs->current->elem = x;
}

void Next(ListIterator ixs) {
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs) {
    ixs->current = NULL;
}

void DisposeIterator(ListIterator ixs) {
    delete ixs;
}

void DestroyL(LinkedList xs) {
    NodoL* actual = xs->primero;
    while (actual != NULL) {
        NodoL* temp =  actual;
        actual = actual->siguiente;
        delete temp;
    }
    delete actual;
    delete xs;
}

