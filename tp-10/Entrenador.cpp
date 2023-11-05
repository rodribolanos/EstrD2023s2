using namespace std;
#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemons;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemons) {
    Entrenador e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemons = pokemons;
    return e;
}

string nombreDeEntrenador(Entrenador e) {
    return e->nombre;
}

int cantidadDePokemon(Entrenador e) {
    return e->cantPokemon;
}

bool esDeTipo(TipoDePokemon tipo, Pokemon p) {
    return tipo == tipoDePokemon(p);
}

int unoSi(bool b) {
    if (b) {
        return 1;
    } else {
        return 0;
    }
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int c = e->cantPokemon;
    int contador = 0;
    for(int i=0; i < c; i++) {
        Pokemon p = e->pokemons[i];
        contador = contador + unoSi(esDeTipo(tipo, p));
    }
    return contador;
}

Pokemon pokemonNro(int i, Entrenador e) {
    if (i > 0 && i <= e->cantPokemon) {
    return e->pokemons[i-1];
  }
}

bool leGanaATodosLosDe(Pokemon p, Entrenador e) {
    bool leGana = false; 
    for (int i=0; i < e->cantPokemon; i++) {
        Pokemon p2 = e->pokemon[i];
        leGana = leGana || superaA(p, p2);
    }
}
bool leGanaATodos(Entrenador e1, Entrenador e2) {
    bool leGana = true;
    for (int i=0; i < e2->cantPokemon; i++) {
        Pokemon p = e2->pokemon[i];
        leGana = leGana && leGanaATodosLosDe(p, e1);
    }
    return leGana;
}
