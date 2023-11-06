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

int pruebaEnt() {
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


int sumatoria(ArrayList xs) {
    int contador = 0;
    for (int i=1; i <= lengthAL(xs); i++ ) {
        contador += get(i, xs);
    }
    return contador;
}

void sucesores(ArrayList xs) {
    for (int i=1; i <= lengthAL(xs); i++) {
        int iesimoElemento = get(i, xs);
        set(i, iesimoElemento +1 , xs);
    }
}

bool pertenece(int x, ArrayList xs) {
    bool belongs = false;
    for (int i=1; !belongs || i <= lengthAL(xs); i++) {
        belongs =  ( x == get(i, xs) );
    }
    return belongs;
}

ArrayList append(ArrayList xs, ArrayList ys) {
    int tamanoPri = lengthAL(xs);
    int tamanoSeg = lengthAL(ys);
    resize(tamanoPri + tamanoSeg, xs); 
    for (int i = 1; i <= tamanoSeg; i++) {
        int elementoAAgregar = get(i, ys);
        add(elementoAAgregar, xs);
    }
    return xs;
}

int elMenor(int x, int y) {
    return (x < y) ? x : y;
}

int minimo(ArrayList xs) {
        int minimo = get(1, xs);
        for (int i=2; i <= lengthAL(xs); i++) {
            minimo = elMenor(minimo, get(i, xs));
        }   
     return minimo;
}
int main() {
    ArrayList array = newArrayList();
    add(5, array);
    add(20, array);
    add(40, array);
    cout << "El primer elemento del array: " << get(1, array) << endl;
    cout << "La longitud del array: " << lengthAL(array) << endl;
    cout << "El 3er elemento es: " << get(3, array) << endl;
    cout << "La sumatoria hasta ahora: " << sumatoria(array) << endl;
    set(3, 25, array);
    cout << "El 3er elemento es: " << get(3, array) << endl;
    cout << "La sumatoria hasta ahora: " << sumatoria(array) << endl;
    sucesores(array);
    cout << "La sumatoria hasta ahora son 3 numeros mas: " << sumatoria(array) << endl;
    cout << "Pertenece el numero 26: " << pertenece(26, array) << endl;
}