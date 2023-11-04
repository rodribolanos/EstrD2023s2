using namespace std;
#include <iostream>
#include "Persona.h"
#include "Pokemon.h"
#include "Entrenador.h"

int prueba1() {
    Persona agustin = consPersona("agustin", 30);
    Persona rodrigo = consPersona("rodri", 20);
    cout << "El nombre Agustin: " << nombre(agustin) << endl;
    cout << "Su edad: " << edad(agustin) << endl;
    crecer(agustin);
    cout << "Edad de Agustin despues de crecer: " << edad(agustin) << endl;
    cambioDeNombre("Agus", agustin);
    cout << "Nombre de Agustin con cambio de nombre: " << nombre(agustin) << endl;
    cout << "Agus es mas grande que Rodri: " << esMayorQueLaOtra(agustin, rodrigo) << endl;
    cout << "Nombre de la persona mayor entre Agus y Rodri: " << nombre(laQueEsMayor(agustin, rodrigo))<< endl;
}

int prueba2() {
    Pokemon pikachu = consPokemon("Agua");
    Pokemon polito = consPokemon("Fuego");
    cout << "El tipo de pikachu: " << tipoDePokemon(pikachu) << endl;
    cout << "La energia de pikachu: "<< energia(pikachu) << endl;
    perderEnergia(25, pikachu);
    cout << "La energia de pikachu despues de batallar: " << energia(pikachu) << endl;
    cout << "Polito supera a pikachu: " << superaA(polito, pikachu) << endl;
    cout << "Pikachu supera a polito: " << superaA(pikachu, polito) << endl;
}

int main() {
    Pokemon* pokes = new Pokemon[1];
    pokes[0] = consPokemon("Agua");
    Pokemon* pokes = new Pokemon[1];
    Entrenador rodri = consEntrenador("rodri", 1, pokes);
    cout << "Nombre: " << nombreDeEntrenador(rodri) << endl;
    cout << "Cantidad de Pokemon: " << cantidadDePokemon(rodri) << endl;
    cout << "Cantidad de Pokemon de Agua: " << cantidadDePokemonDe("Agua", rodri) << endl;
    cout << "Cantidad de Pokemon de Fuego: " << cantidadDePokemonDe("Fuego", rodri) << endl;
    cout << "TipoDePokemon Nro 1: " << tipoDePokemon(pokemonNro(1, rodri)) << endl;
}