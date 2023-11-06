using namespace std;
#include <iostream>
#include "Persona.h"
#include "Pokemon.h"
#include "Entrenador.h"
#include "ArrayList.h"

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
    Pokemon* pokes2 = new Pokemon[2];
    pokes2[0] = consPokemon("Fuego");
    pokes2[1] = consPokemon("Fuego");
    Entrenador rodri = consEntrenador("rodri", 1, pokes);
    Entrenador martu = consEntrenador("Martu", 2, pokes2);
    cout << "Nombre: " << nombreDeEntrenador(rodri) << endl;
    cout << "Cantidad de Pokemon: " << cantidadDePokemon(rodri) << endl;
    cout << "Cantidad de Pokemon de Agua: " << cantidadDePokemonDe("Agua", rodri) << endl;
    cout << "Cantidad de Pokemon de Fuego: " << cantidadDePokemonDe("Fuego", rodri) << endl;
    cout << "TipoDePokemon Nro 1: " << tipoDePokemon(pokemonNro(1, rodri)) << endl;
    cout << "Rodri le gana a todos los de martu: " << leGanaATodos(rodri, martu) << endl;
}

int array() {
    ArrayList array = newArrayList();
    add(5, array);
    cout << "El primer elemento del array: " << get(1, array) << endl;
}