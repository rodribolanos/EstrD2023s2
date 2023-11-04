#include <iostream>
using namespace std;
#include "Fraccion.h"

Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

int numerador(Fraccion f) {
    return f.numerador;
}

int denominador(Fraccion f) {
    return f.denominador;
}

float division(Fraccion f) {
    float resultado = (float) f.numerador / f.denominador ;
    return resultado;
}

Fraccion multF(Fraccion f1, Fraccion f2) {
    Fraccion fNueva;
    fNueva.numerador = f1.numerador * f2.numerador;
    fNueva.denominador = f1.denominador * f2.denominador;

    return fNueva;
}

int dcm(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a; 
}

Fraccion simplificada(Fraccion p) {
    int divisor = dcm(p.numerador, p.denominador);

    return (consFraccion ((p.numerador / divisor), (p.denominador / divisor)));
}

Fraccion sumF(Fraccion f1, Fraccion f2) {
    int multiplo  = (f1.denominador, f2.denominador);
    int num1 = f1.numerador * (multiplo / f1.denominador );
    int num2 = f2.numerador * (multiplo / f2.denominador );
    return consFraccion((num1 + num2), (multiplo));
}


