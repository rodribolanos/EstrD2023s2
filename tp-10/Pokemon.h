using namespace std;
#include <iostream>
typedef string TipoDePokemon;
struct PokeSt;
typedef PokeSt* Pokemon;


Pokemon consPokemon(TipoDePokemon tipo);
//Dado un tipo devuelve un pokémon con 100 % de energía.

TipoDePokemon tipoDePokemon(Pokemon p);
//Devuelve el tipo de un pokémon.

int energia(Pokemon p);
//Devuelve el porcentaje de energía.

void perderEnergia(int energia, Pokemon p);
//Le resta energía al pokémon.

bool superaA(Pokemon p1, Pokemon p2);
// Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
