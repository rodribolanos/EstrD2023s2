#include <iostream>
using namespace std;

struct registroDePar {
    int x;
    int y;
};

typedef struct registroDePar Par;

// Propósito: construye un par
Par consPar(int x, int y);

// Propósito: devuelve la primera componente
int fst(Par p);

// Propósito: devuelve la segunda componente
int snd(Par p);

// Propósito: devuelve la mayor componente
int maxDelPar(Par p);

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p);

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m);
