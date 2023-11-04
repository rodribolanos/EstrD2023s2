#include <iostream>
#include "Par.h"
#include "Fraccion.h"
using namespace std;


int suma() {
    int x = 0;
    int y = 2;
    x = x+y;
    return x;
}

int sumatoria4() {
    int x = 0;
    int y = 0;
    while(y < 5) {
        x += y;
        y++;
    }
    return x;
}

bool cambioBool() {
    int y = 0;
    bool b = true;
    while(b) {
        y++;
        b = !b;
    }
    return b;
}

// Ejercicio 4 con doble implementacion de funciones

// Iterativa

void printN(int n, string s) {
//Propósito: imprime n veces un string s, precondicion = n >0
    while (n > 0) {
        cout << s << endl;
        n = n - 1;
    }
}

// Recursiva
void printN2(int n, string s) {
//Propósito: imprime n veces un string s, precondicion = n >0
    if (n > 0) {
        cout << s << endl;
        printN2 ((n-1), s);
    }
}

void cuentaRegresiva(int n) {
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea
    while (n > 0) {
        cout << n << endl;
        n = n - 1;
    }
}

void cuentaRegresiva2(int n) {
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea
    if (n > 0) {
        cout << n << endl;
        cuentaRegresiva2(n-1);
    }
}

void desdeCeroHastaN(int n) {
    for (int i = 0; i < (n + 1); i++) {
        cout << i << endl;
    }
}

void desdeCeroHastaN2(int n) {
   if (n > -1) { 
        desdeCeroHastaN2 (n-1);
        cout << n << endl;
    }
}
int main() {
    //Par nuevoPar = consPar(10,3);
    //Par division = divisionYResto(10, 3);
    //cout << "Mi par es: " << fst(nuevoPar) << "," << snd(nuevoPar) << endl;
    //cout << "La division de 10 y 3 da " << fst(division) << ", con resto " << snd(division) << endl;      
    //desdeCeroHastaN2(4);
    Fraccion f1 = consFraccion(20, 10);
    Fraccion f2 = consFraccion(30, 20);
    Fraccion f3 = simplificada(multF(f1,f2));
    float f4 = division(f2);
    cout << f4 << endl;
}

/* 1. // Precondición: c1 < c2
 void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}
-- PROPOSITO: Imprime en consola los numeros naturales que esten entre c1 y c2.

2. // Precondición: n >= 0
 int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
}
    return x;
}
-- PROPOSITO: Describe el factorial del numero dado.

3. // Precondición: n <= m
    int ft(int n, int m) {
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m);
}

-- PROPOSITO: Describe la sumatoria de los numeros naturales que esten entre c1 y c2.
*/




