using namespace std;
#include <iostream>
#include "Pokemon.h"


struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

Pokemon consPokemon(TipoDePokemon tipo) {
    Pokemon poke = new PokeSt;
    poke->tipo = tipo;
    poke->vida = 100;  
    return poke; 
}

TipoDePokemon tipoDePokemon(Pokemon p) {
    return p->tipo;
}

int energia(Pokemon p) {
    return p->vida;
}

void perderEnergia(int energia, Pokemon p) {
    p->vida -= energia;
}

bool esSuperiorTipo(string t1, string t2) {
    if (t1 == "Agua" && t2 == "Fuego") {
        return true; 
    } else if (t1 == "Fuego" && t2 == "Planta") {
        return true;
    } else if (t1 == "Planta" && t2 == "Agua") {
        return true;
    } else {
        return false;
    }
}

bool superaA(Pokemon p1, Pokemon p2) {
    return esSuperiorTipo(p1->tipo, p2->tipo);
}


